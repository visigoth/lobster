{-# OPTIONS -Wall #-}
module Main (main) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Char
import Data.Either (partitionEithers)
import Data.Foldable (Foldable, toList)
import Data.List (isSuffixOf)
import Data.Map (Map)
import Data.NonEmptyList (NonEmptyList, FromList(..))
import Data.Set (Set)
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import qualified Data.Map as Map
import qualified Data.Set as Set

import SCD.M4.ModuleFiles
import SCD.M4.PrettyPrint ()
import SCD.M4.Syntax hiding (avPerms)
import qualified SCD.M4.Syntax as M4

import qualified SCD.SELinux.Syntax as S
import qualified Text.Happy.ParserMonad as P

import qualified SCD.Lobster.Gen.CoreSyn as L
import SCD.Lobster.Gen.CoreSyn.Output (showLobster)

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
  , allow_rules      :: !(Map AllowRule [P.Pos])
  , type_transitions :: !(Set (S.TypeId, S.TypeId, S.ClassId, S.TypeId))
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
  modify (f ps)
  where
    rules = [ AllowRule subject object cls perm | perm <- Set.toList perms ]
    f ps st = St
      { object_classes =
          insertMapSet subject processClassId $
          insertMapSet object cls $
          object_classes st
      , class_perms = Map.insertWith (flip Set.union) cls perms (class_perms st)
      , attrib_members = attrib_members st
      , allow_rules = foldr (\r -> Map.insertWith (++) r ps) (allow_rules st) rules
      , type_transitions = type_transitions st
      }

addAttribs :: S.TypeId -> [S.AttributeId] -> M ()
addAttribs typeId attrIds = modify f
  where
    f st = St
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
    f st = St
      { object_classes =
          insertMapSet (S.fromId (S.toId subj)) processClassId $
          insertMapSet (S.fromId (S.toId new)) cls $
          object_classes st
      , class_perms = class_perms st
      , attrib_members = attrib_members st
      , allow_rules = allow_rules st
      , type_transitions = Set.insert (subj, rel, cls, new) (type_transitions st)
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

outputAllowRule :: AllowRule -> L.Decl
outputAllowRule (AllowRule subject object cls perm) =
  L.neutral
     (L.domPort (toIdentifier subject processClassId) activePort)
     (L.domPort (toIdentifier object cls) (toPortId perm))
  where
    toIdentifier :: S.TypeOrAttributeId -> S.ClassId -> L.Name
    toIdentifier typeId classId = L.Name (lowercase (S.idString typeId ++ "__" ++ S.idString classId))

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
    toIdentifier :: S.TypeOrAttributeId -> S.ClassId -> L.Name
    toIdentifier typeId classId = L.Name (lowercase (S.idString typeId ++ "__" ++ S.idString classId))

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
    toIdentifier :: S.TypeId -> S.ClassId -> L.Name
    toIdentifier typeId classId = L.Name (lowercase (S.idString typeId ++ "__" ++ S.idString classId))

    activePort :: L.Name
    activePort = L.Name "active"

    transitionPort :: L.Name
    transitionPort = L.Name "type_transition"

outputLobster :: M4.Policy -> St -> [L.Decl]
outputLobster policy st =
  classDecls policy st ++ domainDecls ++ connectionDecls ++ attributeDecls ++ transitionDecls
  where
    toClassId :: S.ClassId -> L.Name
    toClassId = L.Name . capitalize . S.idString

    toIdentifier :: S.TypeOrAttributeId -> S.ClassId -> L.Name
    toIdentifier typeId classId = L.Name (lowercase (S.idString typeId ++ "__" ++ S.idString classId))

    domainDecls :: [L.Decl]
    domainDecls =
      [ L.Domain (toIdentifier typeId classId) (toClassId classId) []
      | (typeId, classIds) <- Map.assocs (object_classes st)
      , classId <- Set.toList classIds
      ]

    connectionDecls :: [L.Decl]
    connectionDecls = map outputAllowRule (Map.keys (allow_rules st))

    attributeDecls :: [L.Decl]
    attributeDecls = do
      (ty, attrs) <- Map.assocs (attrib_members st)
      attr <- Set.toList attrs
      outputAttributes st (S.fromId (S.toId ty)) (S.fromId (S.toId attr))

    transitionDecls :: [L.Decl]
    transitionDecls = map outputTypeTransition (Set.toList (type_transitions st))

----------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  (opts, iDir) <- checkOpt_ args
  policy0 <- readPolicy (ifdefDeclFile opts) iDir
  let patternMacros =
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
  putStrLn $ showLobster (outputLobster policy finalSt)

----------------------------------------------------------------------
-- Selective macro expansion

data Macros = Macros
  { stmtMacros :: Map M4Id [Stmt]
  , identMacros :: Map M4Id [S.Identifier]
  }

class Expand a where
  expand :: Macros -> a -> a
  expand s x = fromSingle (expandList s x)

  expandList :: Macros -> a -> [a]
  expandList s x = [expand s x]

instance Expand a => Expand [a] where
  expand s = concatMap (expandList s)

instance Expand a => Expand (NonEmptyList a) where
  expand s = fromList . concatMap (expandList s) . toList

expandListId :: S.IsIdentifier i => Macros -> i -> [i]
expandListId s x =
  case Map.lookup (S.fromId (S.toId x)) (identMacros s) of
    Nothing -> [x]
    Just xs -> map S.fromId xs

instance Expand S.RoleId            where expandList = expandListId
instance Expand S.AttributeId       where expandList = expandListId
instance Expand S.TypeOrAttributeId where expandList = expandListId
instance Expand S.BoolId            where expandList = expandListId
instance Expand S.ClassId           where expandList = expandListId
instance Expand S.TypeId            where expandList = expandListId
instance Expand S.PermissionId      where expandList = expandListId

instance Expand a => Expand (S.SignedId a) where
  expandList s (S.SignedId sig x) = map (S.SignedId sig) (expandList s x)

instance Expand i => Expand (S.StarTilde i) where
  expand _ S.Star = S.Star
  expand s (S.Tilde xs) = S.Tilde (expand s xs)

instance Expand S.Permissions where
  expand s (S.Permissions pids) = S.Permissions (expand s pids)
  expand s (S.PStarTilde st) = S.PStarTilde (expand s st)

instance (Expand st, Expand tt) => Expand (S.SourceTarget st tt) where
  expand s (S.SourceTarget st tt tc) = S.SourceTarget (expand s st) (expand s tt) (expand s tc)

instance Expand t => Expand (S.NeverAllow t) where
  expand s (S.NeverAllow xs) = S.NeverAllow (expand s xs)
  expand s (S.NAStarTilde st) = S.NAStarTilde (expand s st)

instance Expand S.Self where
  expandList _ S.Self = [S.Self]
  expandList s (S.NotSelf i) = map S.NotSelf (expandList s i)

instance Expand Stmt where
  expandList s stmt =
    case stmt of
      Tunable cond stmts1 stmts2  -> [Tunable cond (expand s stmts1) (expand s stmts2)]
      Optional stmts1 stmts2      -> [Optional (expand s stmts1) (expand s stmts2)]
      Ifdef i stmts1 stmts2       -> [Ifdef i (expand s stmts1) (expand s stmts2)]
      Ifndef i stmts              -> [Ifndef i (expand s stmts)]
      RefPolicyWarn _             -> [stmt]
      Call i args                 ->
        case Map.lookup i (stmtMacros s) of
          Nothing -> [stmt]
          Just stmts -> expand s $ map (substStmt args) stmts
      Role r attrs                -> [Role (expand s r) (expand s attrs)]
      AttributeRole attr          -> [AttributeRole (expand s attr)]
      RoleAttribute r attrs       -> [RoleAttribute (expand s r) (expand s attrs)]
      RoleTransition rs ts r      -> [RoleTransition (expand s rs) (expand s ts) (expand s r)]
      RoleAllow rs1 rs2           -> [RoleAllow (expand s rs1) (expand s rs2)]
      Attribute attr              -> [Attribute (expand s attr)]
      Type t ts attrs             -> [Type (expand s t) (expand s ts) (expand s attrs)]
      TypeAlias t ts              -> [TypeAlias (expand s t) (expand s ts)]
      TypeAttribute t attrs       -> [TypeAttribute (expand s t) (expand s attrs)]
      RangeTransition xs ys zs (MlsRange a b)
                                  -> [RangeTransition (expand s xs) (expand s ys) (expand s zs)
                                                          (MlsRange (id a) (id b))]
      TeNeverAllow st perms       -> [TeNeverAllow (expand s st) (expand s perms)]
      Transition tr st t          -> [Transition tr (expand s st) (expand s t)]
      TeAvTab ad st perms         -> [TeAvTab ad (expand s st) (expand s perms)]
      CondStmt cond stmts1 stmts2 -> [CondStmt cond (expand s stmts1) (expand s stmts2)]
      XMLDocStmt _                -> [stmt]
      SidStmt _                   -> [stmt]
      FileSystemUseStmt _         -> [stmt]
      GenFileSystemStmt _         -> [stmt]
      PortStmt _                  -> [stmt]
      NetInterfaceStmt _          -> [stmt]
      NodeStmt _                  -> [stmt]
      Define _i                   -> [stmt]
      Require _reqs               -> [stmt]
      GenBoolean t i b            -> [GenBoolean t (expand s i) b]
      StmtPosition stmt1 pos      -> [StmtPosition stmt' pos | stmt' <- expandList s stmt1]

instance Expand InterfaceElement where
  expand s (InterfaceElement ty doc i stmts) =
    InterfaceElement ty doc i (expand s stmts)

instance Expand Interface where
  expand s (InterfaceModule doc es) = InterfaceModule doc (expand s es)

instance Expand Implementation where
  expand s (Implementation i v stmts) = Implementation i v (expand s stmts)

instance Expand PolicyModule where
  expand s pm =
    pm { interface = expand s (interface pm)
       , implementation = expand s (implementation pm)
       }

expandPolicyModule :: Macros -> PolicyModule -> PolicyModule
expandPolicyModule = expand

----------------------------------------------------------------------
-- Substitution for $1, $2, $3 ...

type Substitution = [NonEmptyList (S.SignedId S.Identifier)]

fromSingle :: (Foldable l) => l a -> a
fromSingle ys =
  case toList ys of
    [y] -> y
    _ -> error "fromSingle"

fromPositive :: S.SignedId a -> a
fromPositive (S.SignedId S.Positive x) = x
fromPositive (S.SignedId S.Negative _) = error "fromPositive"

class Subst a where
  subst :: Substitution -> a -> a
  subst s x = fromSingle (substList s x)

  substList :: Substitution -> a -> [a]
  substList s x = map fromPositive (substListSigned s x)

  substListSigned :: Substitution -> a -> [S.SignedId a]
  substListSigned s x = [S.SignedId S.Positive (subst s x)]

instance Subst a => Subst (S.SignedId a) where
  substList s (S.SignedId S.Positive x) = substListSigned s x
  substList s (S.SignedId S.Negative x) =
    fmap (S.SignedId S.Negative . fromPositive) (substListSigned s x)

getArg :: Substitution -> Int -> [S.SignedId S.Identifier]
getArg s n | n <= length s = toList (s !! (n - 1))
           | otherwise     = []

substString :: Substitution -> String -> String
substString s ('$':cs) =
  S.idString (fromPositive (fromSingle (getArg s (read ds)))) ++ substString s cs'
    where (ds, cs') = span isDigit cs
substString s (c : cs) = c : substString s cs
substString _ [] = []

asDollar :: String -> Maybe Int
asDollar ('$' : s) | all isDigit s = Just (read s)
asDollar _ = Nothing

substId :: S.IsIdentifier i => Substitution -> i -> [S.SignedId i]
substId s i =
  case asDollar (S.idString i) of
    Nothing -> return (S.SignedId S.Positive (S.mkId (substString s (S.idString i))))
    Just n -> fmap (fmap S.fromId) (getArg s n)

instance Subst S.Identifier        where substListSigned = substId
instance Subst S.TypeOrAttributeId where substListSigned = substId
instance Subst S.PermissionId      where substListSigned = substId
instance Subst S.BoolId            where substListSigned = substId
instance Subst S.ClassId           where substListSigned = substId
instance Subst S.TypeId            where substListSigned = substId
instance Subst S.AttributeId       where substListSigned = substId
instance Subst S.RoleId            where substListSigned = substId

instance Subst S.Self where
  substList _ S.Self = [S.Self]
  substList s (S.NotSelf i) = map S.NotSelf (substList s i)
  substListSigned _ S.Self = [S.SignedId S.Positive S.Self]
  substListSigned s (S.NotSelf i) = map (fmap S.NotSelf) (substListSigned s i)

instance Subst a => Subst [a] where
  subst s xs = concatMap (substList s) xs

instance Subst a => Subst (NonEmptyList a) where
  subst s xs = fromList (concatMap (substList s) (toList xs))

instance Subst S.CondExpr where
  subst s (S.Not c) = S.Not (subst s c)
  subst s (S.Op c1 o c2) = S.Op (subst s c1) o (subst s c2)
  subst s (S.Var i) = S.Var (subst s i)

instance Subst i => Subst (S.StarTilde i) where
  subst _ S.Star = S.Star
  subst s (S.Tilde xs) = S.Tilde (subst s xs)

instance Subst S.Permissions where
  subst s (S.Permissions pids) = S.Permissions (subst s pids)
  subst s (S.PStarTilde st) = S.PStarTilde (subst s st)

instance (Subst st, Subst tt) => Subst (S.SourceTarget st tt) where
  subst s (S.SourceTarget st tt tc) = S.SourceTarget (subst s st) (subst s tt) (subst s tc)

instance Subst t => Subst (S.NeverAllow t) where
  subst s (S.NeverAllow xs) = S.NeverAllow (subst s xs)
  subst s (S.NAStarTilde st) = S.NAStarTilde (subst s st)

instance Subst M4.Stmt where
  subst s stmt =
    case stmt of
      Tunable c stmts1 stmts2 -> Tunable (subst s c) (subst s stmts1) (subst s stmts2)
      Optional stmts1 stmts2  -> Optional (subst s stmts1) (subst s stmts2)
      Ifdef i stmts1 stmts2   -> Ifdef i (subst s stmts1) (subst s stmts2)
      Ifndef i stmts          -> Ifndef i (subst s stmts)
      RefPolicyWarn _         -> stmt
      Call i args | isDS args -> Call i s
                  | otherwise -> Call i (subst s args)
      Role r attrs            -> Role (subst s r) (subst s attrs)
      AttributeRole attr      -> AttributeRole (subst s attr)
      RoleAttribute r attrs   -> RoleAttribute (subst s r) (subst s attrs)
      RoleTransition rs ts r  -> RoleTransition (subst s rs) (subst s ts) (subst s r)
      RoleAllow rs1 rs2       -> RoleAllow (subst s rs1) (subst s rs2)
      Attribute attr          -> Attribute (subst s attr)
      Type t ts attrs         -> Type (subst s t) (subst s ts) (subst s attrs)
      TypeAlias t ts          -> TypeAlias (subst s t) (subst s ts)
      TypeAttribute t attrs   -> TypeAttribute (subst s t) (subst s attrs)
      RangeTransition xs ys zs (MlsRange a b)
                              -> RangeTransition (subst s xs) (subst s ys) (subst s zs)
                                 (MlsRange (id a) (id b))
      TeNeverAllow st perms   -> TeNeverAllow (subst s st) (subst s perms)
      Transition tr st t      -> Transition tr (subst s st) (subst s t)
      TeAvTab ad st perms     -> TeAvTab ad (subst s st) (subst s perms)
      CondStmt c ss1 ss2      -> CondStmt c (subst s ss1) (subst s ss2)
      XMLDocStmt _            -> stmt
      SidStmt _               -> stmt --FIXME
      FileSystemUseStmt _     -> stmt --FIXME
      GenFileSystemStmt _     -> stmt --FIXME
      PortStmt _              -> stmt --FIXME
      NetInterfaceStmt _      -> stmt --FIXME
      NodeStmt _              -> stmt --FIXME
      Define _i               -> stmt --FIXME
      Require _reqs           -> stmt --FIXME
      GenBoolean t i b        -> GenBoolean t (subst s i) b
      StmtPosition stmt1 pos  -> StmtPosition (subst s stmt1) pos
    where
      isDS = (==) [fromList [S.SignedId S.Positive (S.mkId "$*")]]

substStmt :: Substitution -> Stmt -> Stmt
substStmt = subst

----------------------------------------------------------------------
-- option handling

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

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["multiple"] (ReqArg (\a o -> o{ path = a, isDir = True }) "FILE") ""
  , Option [] ["single"] (ReqArg (\a o -> o{ path = a, isDir = False }) "FILE") ""
  , Option [] ["infer-missing"] (NoArg (\o -> o{ inferMissing = True })) ""
  , Option [] ["ifdefs"] (ReqArg (\f o -> o{ ifdefDeclFile = Just f }) "FILE") ""
--   , Option [] ["kind-errors"] (NoArg (\o -> o{ kindErrors = True })) ""
  ]

printUsage :: IO ()
printUsage = do
  p <- getProgName
  putStrLn $ unlines $
    [ "Usage:"
    , p ++ "[global options] <input directory>"
    , "  Generate lobster policy module(s) from selinux policy in <input directory>"
    , "Global options:"
    , "--single <file>    Place all lobster definitions in a single <file>"
    , "--multiple <dir>   Generate multiple lobster files and place them in <dir>"
    , "--ifdefs <file>    Read ifdef declarations from <file>"
    , "--infer-missing    infer missing classes (suitable for graphing by lviz)"
--     , "--kind-errors      Output errors generated by kind checking"
    ]


-- main helper functions

checkOpt_ :: [String] -> IO (Options,FilePath)
checkOpt_ args = do
  (opts,fns) <- checkOpt options defaultOptions args
  case fns of
    [a] -> do
      x <- canonicalizePath a
      return (opts, x)
    _ -> do
      pn <- getProgName
      exitErrors ["expecting: " ++ pn ++ " <input directory>"]

checkOpt :: [OptDescr (a -> a)] -> a -> [String] -> IO (a,[String])
checkOpt os d args =
  case getOpt Permute os args of
    (f, r, [])   -> return (foldl (flip id) d f, r)
    (_, _, errs) -> exitErrors errs

reportErrors :: [String] -> IO ()
reportErrors errs = do
  p <- getProgName
  putStrLn (p ++ ":" ++ concat errs)

exitErrors :: [String] -> IO a
exitErrors errs = do
  reportErrors errs
  printUsage
  exitFailure

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

lowercase :: String -> String
lowercase "domain" = "domain_type" -- FIXME: ugly hack
lowercase "" = ""
lowercase (x:xs) = toLower x : xs
