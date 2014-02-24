{-# OPTIONS -Wall #-}
module M4ToLobster where

import Control.Error
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Char
import Data.Foldable (toList)
import Data.List (isSuffixOf)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import SCD.M4.ModuleFiles
import SCD.M4.PrettyPrint ()
import SCD.M4.Syntax hiding (avPerms)
import qualified SCD.M4.Syntax as M4

import qualified SCD.SELinux.Syntax as S
import qualified Text.Happy.ParserMonad as P

import qualified SCD.Lobster.Gen.CoreSyn as L

import SCD.M4.Subst (Macros(..), expandPolicyModule)

data Options = Options
  { path :: FilePath
  , isDir :: Bool
  , ifdefDeclFile :: Maybe FilePath
  , inferMissing :: Bool
--   , kindErrors :: Bool
  } deriving Show

-- | Default options for reference policy processing
defaultOptions :: Options
defaultOptions = Options
  { path = "Gen_Lobster_Dir"
  , isDir = True
  , ifdefDeclFile = Nothing
  , inferMissing = False
--   , kindErrors = False
  }

----------------------------------------------------------------------
-- State monad

data AllowRule = AllowRule
  { allowSubject    :: S.TypeOrAttributeId
  , allowObject     :: S.TypeOrAttributeId
  , allowClass      :: S.ClassId
  , allowPerm       :: S.PermissionId
  } deriving (Eq, Ord, Show)

data St = St
  { object_classes   :: !(Map S.TypeOrAttributeId (Set S.ClassId))
  , class_perms      :: !(Map S.ClassId (Set S.PermissionId))
  , attrib_members   :: !(Map S.TypeId (Set S.AttributeId))
  , allow_rules      :: !(Map AllowRule (Set P.Pos))
  , type_transitions :: !(Set (S.TypeId, S.TypeId, S.ClassId, S.TypeId))
  , domtrans_macros  :: !(Set (S.TypeOrAttributeId, S.TypeOrAttributeId, S.TypeOrAttributeId))
  }

processClassId :: S.ClassId
processClassId = S.mkId "process"

activePermissionId :: S.PermissionId
activePermissionId = S.mkId "active"

initSt :: St
initSt = St
  { object_classes   = Map.empty
  , class_perms      = Map.singleton processClassId (Set.singleton activePermissionId)
  , attrib_members   = Map.empty
  --, attrib_members = Set.empty
  , allow_rules      = Map.empty
  , type_transitions = Set.empty
  , domtrans_macros  = Set.empty
  }

type M a = ReaderT [P.Pos] (State St) a

----------------------------------------------------------------------
-- Processing of M4 policy

filterSignedId :: Eq a => [S.SignedId a] -> [a]
filterSignedId xs = [ y | y <- ys, y `notElem` zs ]
  where
    (ys, zs) = partitionEithers (map f xs)
    f (S.SignedId S.Positive y) = Left y
    f (S.SignedId S.Negative z) = Right z

fromSelf :: S.TypeOrAttributeId -> S.Self -> S.TypeOrAttributeId
fromSelf x S.Self = x
fromSelf _ (S.NotSelf x) = x

insertMapSet :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
insertMapSet k x = Map.insertWith (flip Set.union) k (Set.singleton x)

addAllow :: S.TypeOrAttributeId -> S.TypeOrAttributeId
         -> S.ClassId -> Set S.PermissionId -> M ()
addAllow subject object cls perms = do
  ps <- ask
  -- discard all but the outermost enclosing source position
  -- so that we only get the position of the top-level macro
  modify (f (Set.fromList (take 1 (reverse ps))))
  where
    rules = [ AllowRule subject object cls perm | perm <- Set.toList perms ]
    f ps st = st
      { object_classes =
          insertMapSet subject processClassId $
          insertMapSet object cls $
          object_classes st
      , class_perms = Map.insertWith (flip Set.union) cls perms (class_perms st)
      , attrib_members = attrib_members st
      , allow_rules = foldr (\r -> Map.insertWith (flip Set.union) r ps) (allow_rules st) rules
      , type_transitions = type_transitions st
      }

addAttribs :: S.TypeId -> [S.AttributeId] -> M ()
addAttribs typeId attrIds = modify f
  where
    f st = st
      { object_classes = object_classes st
      , class_perms = class_perms st
      , attrib_members =
          Map.insertWith (flip Set.union) typeId (Set.fromList attrIds) $
          attrib_members st
      , allow_rules = allow_rules st
      , type_transitions = type_transitions st
      }

addTypeTransition :: S.TypeId -> S.TypeId -> S.ClassId -> S.TypeId -> M ()
addTypeTransition subj rel cls new = modify f
  where
    f st = st
      { object_classes =
          insertMapSet (S.fromId (S.toId subj)) processClassId $
          insertMapSet (S.fromId (S.toId new)) cls $
          object_classes st
      , class_perms = class_perms st
      , attrib_members = attrib_members st
      , allow_rules = allow_rules st
      , type_transitions = Set.insert (subj, rel, cls, new) (type_transitions st)
      }

addDomtransMacro :: S.TypeOrAttributeId -> S.TypeOrAttributeId -> S.TypeOrAttributeId -> M ()
addDomtransMacro d1 d2 d3 = modify f
  where
    f st = st
      { object_classes =
          insertMapSet d1 processClassId $
          insertMapSet d1 (S.mkId "fd") $
          insertMapSet d1 (S.mkId "fifo_file") $
          insertMapSet d2 (S.mkId "file") $
          insertMapSet d3 processClassId $
          object_classes st
      , class_perms =
          insertMapSet (S.mkId "file") (S.mkId "x_file_perms") $
          class_perms st
      , attrib_members = attrib_members st
      , domtrans_macros = Set.insert (d1, d2, d3) (domtrans_macros st)
      }

isDefined :: M4.IfdefId -> Bool
isDefined _ = False
-- ^ FIXME: make this depend on a parameter

processStmts :: M4.Stmts -> M ()
processStmts = mapM_ processStmt

processStmt :: M4.Stmt -> M ()
processStmt stmt =
  case stmt of
    Ifdef i stmts1 stmts2 -> processStmts (if isDefined i then stmts1 else stmts2)
    Ifndef i stmts -> processStmts (if isDefined i then [] else stmts)
    Type t _aliases attrs -> addAttribs t attrs -- TODO: track aliases
    TypeAttribute t attrs -> addAttribs t (toList attrs)
    Transition S.TypeTransition (S.SourceTarget al bl cl) t ->
      sequence_ $
        [ addTypeTransition subject related classId object
        | let object = (S.fromId . S.toId) t
        , subject <- map (S.fromId . S.toId) $ filterSignedId $ toList al
        , related <- map (S.fromId . S.toId) $ filterSignedId $ toList bl
        , classId <- toList cl
        ]
    TeAvTab S.Allow (S.SourceTarget al bl cl) (S.Permissions dl) ->
      sequence_ $
        [ addAllow subject object classId perms
        | subject <- filterSignedId $ toList al
        , object' <- filterSignedId $ toList bl
        , let object = fromSelf subject object'
        , classId <- toList cl
        , let perms = Set.fromList $ toList dl
        ]
    StmtPosition stmt1 pos -> local (pos :) $ processStmt stmt1
    Call m4id [al, bl, cl] | m4id == S.mkId "domtrans_pattern" ->
      sequence_ $
        [ addDomtransMacro a b c
        | a <- map (S.fromId . S.toId) $ filterSignedId $ toList al
        , b <- map (S.fromId . S.toId) $ filterSignedId $ toList bl
        , c <- map (S.fromId . S.toId) $ filterSignedId $ toList cl
        ]
    _ -> return ()

processImplementation :: M4.Implementation -> M ()
processImplementation (M4.Implementation _ _ stmts) = mapM_ processStmt stmts

processPolicyModule :: M4.PolicyModule -> M ()
processPolicyModule m = processImplementation (M4.implementation m)

processPolicy :: M4.Policy -> M ()
processPolicy policy = mapM_ processPolicyModule (M4.policyModules policy)

----------------------------------------------------------------------
-- Generation of Lobster code

classPermissions :: Policy -> Map S.ClassId (Set S.PermissionId)
classPermissions policy =
  Map.fromList [ (i, perms x) | S.AvPermClass i x <- toList (M4.avPerms policy) ]
  where
    commonMap = Map.fromList [ (i, toList ps) | S.CommonPerm i ps <- commonPerms policy ]
    perms (Left ps) = Set.fromList (toList ps)
    perms (Right (i, ps)) = Set.fromList (Map.findWithDefault [] i commonMap ++ ps)

classDecls :: M4.Policy -> St -> [L.Decl]
classDecls policy st = map classDecl (Map.assocs permissionMap)
  where
    permissionMap :: Map S.ClassId (Set S.PermissionId)
    permissionMap = Map.unionWith Set.union (class_perms st) (classPermissions policy)

    classDecl :: (S.ClassId, Set S.PermissionId) -> L.Decl
    classDecl (classId, perms) = L.Class (toClassId classId) [] (header ++ stmts)
      where
        header = map L.newPort [memberPort, attributePort, transitionPort]
        stmts = [ L.newPort (toPortId p) | p <- Set.toList perms ]

    memberPort :: L.Name
    memberPort = L.Name "member"

    attributePort :: L.Name
    attributePort = L.Name "attribute"

    transitionPort :: L.Name
    transitionPort = L.Name "type_transition"

    toPortId :: S.PermissionId -> L.Name
    toPortId = L.Name . lowercase . S.idString

    toClassId :: S.ClassId -> L.Name
    toClassId = L.Name . capitalize . S.idString

toIdentifier :: S.IsIdentifier i => i -> S.ClassId -> L.Name
toIdentifier typeId classId = L.Name (lowercase (S.idString typeId ++ "__" ++ S.idString classId))

outputPos :: P.Pos -> L.ConnectAnnotation
outputPos (P.Pos fname _ l c) =
  L.ConnectAnnotation (L.Name "SourcePos")
    [L.AnnotationString fname, L.AnnotationInt l, L.AnnotationInt c]

outputAllowRule :: (AllowRule, Set P.Pos) -> L.Decl
outputAllowRule (AllowRule subject object cls perm, ps) =
  L.connect' L.N
    (L.domPort (toIdentifier subject processClassId) activePort)
    (L.domPort (toIdentifier object cls) (toPortId perm))
    (map outputPos (Set.toList ps))
  where
    activePort :: L.Name
    activePort = L.Name "active"

    toPortId :: S.PermissionId -> L.Name
    toPortId = L.Name . lowercase . S.idString

outputAttribute :: S.TypeOrAttributeId -> S.TypeOrAttributeId -> S.ClassId -> L.Decl
outputAttribute ty attr cls =
  L.neutral
    (L.domPort (toIdentifier ty cls) memberPort)
    (L.domPort (toIdentifier attr cls) attributePort)
  where
    memberPort :: L.Name
    memberPort = L.Name "member"

    attributePort :: L.Name
    attributePort = L.Name "attribute"

outputAttributes :: St -> S.TypeOrAttributeId -> S.TypeOrAttributeId -> [L.Decl]
outputAttributes st ty attr = [ outputAttribute ty attr cls | cls <- classes ]
  where
    classesOf t = Map.findWithDefault Set.empty t (object_classes st)
    classes = Set.toList $ Set.intersection (classesOf ty) (classesOf attr)

outputTypeTransition :: (S.TypeId, S.TypeId, S.ClassId, S.TypeId) -> L.Decl
outputTypeTransition (subj, rel, cls, new) =
  L.connect' L.N
    (L.domPort (toIdentifier subj processClassId) activePort)
    (L.domPort (toIdentifier new cls) transitionPort)
    [L.ConnectAnnotation (L.Name "TypeTransition") [L.AnnotationString (S.idString rel)]]
  where
    activePort :: L.Name
    activePort = L.Name "active"

    transitionPort :: L.Name
    transitionPort = L.Name "type_transition"

outputDomtransMacro ::
  Int -> (S.TypeOrAttributeId, S.TypeOrAttributeId, S.TypeOrAttributeId) -> [L.Decl]
outputDomtransMacro n (d1, d2, d3) =
  [ L.Domain d (L.Name "Domtrans_pattern") [L.Name (show (S.idString d2))]
  , L.neutral
      (L.domPort (toId d1 "process") (L.Name "active"))
      (L.domPort d (L.Name "d1_active"))
  , L.neutral
      (L.domPort (toId d1 "fd") (L.Name "use"))
      (L.domPort d (L.Name "d1_fd_use"))
  , L.neutral
      (L.domPort (toId d1 "fifo_file") (L.Name "rw_fifo_file_perms"))
      (L.domPort d (L.Name "d1_fifo"))
  , L.neutral
      (L.domPort (toId d1 "process") (L.Name "sigchld"))
      (L.domPort d (L.Name "d1_sigchld"))
  , L.neutral
      (L.domPort (toId d2 "file") (L.Name "x_file_perms"))
      (L.domPort d (L.Name "d2"))
  , L.neutral
      (L.domPort (toId d3 "process") (L.Name "active"))
      (L.domPort d (L.Name "d3_active"))
  , L.neutral
      (L.domPort (toId d3 "process") (L.Name "transition"))
      (L.domPort d (L.Name "d3_transition"))
  , L.neutral
      (L.domPort (toId d3 "process") (L.Name "type_transition"))
      (L.domPort d (L.Name "d3_type_transition"))
  ]
  where
    toId :: S.TypeOrAttributeId -> String -> L.Name
    toId typeId classId = L.Name (lowercase (S.idString typeId ++ "__" ++ classId))

    d :: L.Name
    d = L.Name ("domtrans" ++ show n)

outputLobster :: M4.Policy -> St -> [L.Decl]
outputLobster policy st =
  domtransDecl :
  classDecls policy st ++ domainDecls ++ connectionDecls ++ attributeDecls ++ transitionDecls
    ++ domtransDecls
  where
    toClassId :: S.ClassId -> L.Name
    toClassId = L.Name . capitalize . S.idString

    domainDecls :: [L.Decl]
    domainDecls =
      [ L.Domain (toIdentifier typeId classId) (toClassId classId) []
      | (typeId, classIds) <- Map.assocs (object_classes st)
      , classId <- Set.toList classIds
      ]

    connectionDecls :: [L.Decl]
    connectionDecls = map outputAllowRule (Map.assocs (allow_rules st))

    attributeDecls :: [L.Decl]
    attributeDecls = do
      (ty, attrs) <- Map.assocs (attrib_members st)
      attr <- Set.toList attrs
      outputAttributes st (S.fromId (S.toId ty)) (S.fromId (S.toId attr))

    transitionDecls :: [L.Decl]
    transitionDecls = map outputTypeTransition (Set.toList (type_transitions st))

    domtransDecls :: [L.Decl]
    domtransDecls = concat $ zipWith outputDomtransMacro [1..] (Set.toList (domtrans_macros st))

    domtransDecl :: L.Decl
    domtransDecl =
      L.Class (L.Name "Domtrans_pattern") [d2_name]
        [ L.newPort d1_active
        , L.newPort d1_fd_use
        , L.newPort d1_fifo
        , L.newPort d1_sig
        , L.newPort d2
        , L.newPort d3_active
        , L.newPort d3_trans
        , L.newPort d3_tt
        , L.neutral (L.extPort d1_active) (L.extPort d2)
        , L.neutral (L.extPort d1_active) (L.extPort d3_trans)
        , L.connect' L.N (L.extPort d1_active) (L.extPort d3_tt)
            [L.ConnectAnnotation (L.Name "TypeTransition") [L.AnnotationVar d2_name]]

        , L.neutral (L.extPort d3_active) (L.extPort d1_fd_use)
        , L.neutral (L.extPort d3_active) (L.extPort d1_fifo)
        , L.neutral (L.extPort d3_active) (L.extPort d1_sig)
        ]
      where
        d1_active = L.Name "d1_active"
        d1_fd_use = L.Name "d1_fd_use"
        d1_fifo   = L.Name "d1_fifo"
        d1_sig    = L.Name "d1_sigchld"
        d2        = L.Name "d2"
        d3_active = L.Name "d3_active"
        d3_trans  = L.Name "d3_transition"
        d3_tt     = L.Name "d3_type_transition"
        d2_name   = L.Name "d2_name"

----------------------------------------------------------------------

-- | Placeholder error type.  This should be split out into
-- a different module and have multiple constructors for each
-- type of error.
data Error = Error String
  deriving (Eq, Ord, Show)

-- | Handle an error in the I/O monad when running the command line
-- program.
handleError :: Error -> IO a
handleError (Error s) = error s

-- | Run an "EitherT Error IO a" action, handling errors with
-- "handleError", otherwise returning the "a" in the IO monad.
--
-- This is a convenience wrapper around "runEitherT"
-- for use in the command line tool.
runErr :: EitherT Error IO a -> IO a
runErr = eitherT handleError return

-- | Run an IO action, catching exceptions and returning them
-- as an "Error".  Use this instead of "liftIO".
runIO :: IO a -> EitherT Error IO a
runIO f = fmapLT (Error . show) (tryIO f)

-- | Parse a directory containing an SELinux reference policy
-- into Lobster.  If we encounter an error along the way,
-- discard the translation and return the error.
--
-- TODO: Return multiple errors if we can find them.
-- TODO: Redefine "main" in terms of this.
dirToLobster :: FilePath -> Options -> EitherT Error IO [L.Decl]
dirToLobster iDir opts = do
  policy0 <- runIO $ readPolicy (ifdefDeclFile opts) iDir
  hoistEither $ toLobster policy0

-- | Convert a policy to Lobster.
toLobster :: Policy -> Either Error [L.Decl]
toLobster policy0 = do
  let patternMacros =
        -- We handle domtrans_pattern macro as a special case, for now
        Map.delete (S.mkId "domtrans_pattern") $
        Map.fromList
          [ (i, reverse stmts) | SupportDef i stmts <- supportDefs policy0 ]
        -- ^ Policy pattern macros are parsed with statements in reverse order
  let interfaceMacros =
        Map.fromList
          [ (i, stmts)
          | m <- policyModules policy0
          , InterfaceElement InterfaceType _doc i stmts <- interfaceElements (interface m) ]
  let templateMacros =
        Map.fromList
          [ (i, stmts)
          | m <- policyModules policy0
          , InterfaceElement TemplateType _doc i stmts <- interfaceElements (interface m) ]
  let classSetMacros =
        Map.fromList
          [ (i, toList ids)
          | ClassPermissionDef i ids _ <- classPermissionDefs policy0
          , "_class_set" `isSuffixOf` S.idString i ]
  let macros = Macros (Map.unions [patternMacros, interfaceMacros, templateMacros]) classSetMacros
  let policy = policy0 { policyModules = map (expandPolicyModule macros) (policyModules policy0) }
  let finalSt = execState (runReaderT (processPolicy policy) []) initSt
  return (outputLobster policy finalSt)

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

lowercase :: String -> String
lowercase "domain" = "domain_type" -- FIXME: ugly hack
lowercase "" = ""
lowercase (x:xs) = toLower x : xs
