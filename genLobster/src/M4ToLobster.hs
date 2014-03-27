{-# OPTIONS -Wall -Werror #-}
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
import qualified Data.MapSet as MapSet

import SCD.M4.PrettyPrint ()
import SCD.M4.Syntax hiding (avPerms)
import qualified SCD.M4.Syntax as M4

import qualified SCD.SELinux.Syntax as S
import qualified Text.Happy.ParserMonad as P

import qualified CoreSyn as L
import qualified Lobster.Core as C

import SCD.M4.Subst (Macros(..), expandPolicyModule)

import M4ToLobster.Error

----------------------------------------------------------------------
-- State monad

data AllowRule = AllowRule
  { allowSubject    :: S.TypeOrAttributeId
  , allowObject     :: S.TypeOrAttributeId
  , allowClass      :: S.ClassId
  , allowConditions :: [(Either M4.IfdefId S.CondExpr, Bool)]
  } deriving (Eq, Ord, Show)

data St = St
  { object_classes   :: !(Map S.TypeOrAttributeId (Set S.ClassId))
  , class_perms      :: !(Map S.ClassId (Set S.PermissionId))
  , attrib_members   :: !(Map S.AttributeId (Set S.TypeId))
  , allow_rules      :: !(Map AllowRule (Map S.PermissionId (Set P.Pos)))
  , type_transitions :: !(Set (S.TypeId, S.TypeId, S.ClassId, S.TypeId))
  , domtrans_macros  :: !(Set (S.Identifier, [S.TypeOrAttributeId]))
  , type_modules     :: !(Map S.Identifier M4.ModuleId)
  , unique_supply    :: Int
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
  , allow_rules      = Map.empty
  , type_transitions = Set.empty
  , domtrans_macros  = Set.empty
  , type_modules     = Map.empty
  , unique_supply    = 0
  }

data Env = Env
  { envPositions :: [P.Pos]
  , envConditions :: [(Either M4.IfdefId S.CondExpr, Bool)]
  , envModuleId :: M4.ModuleId
  }

initEnv :: Env
initEnv = Env
  { envPositions = []
  , envConditions = []
  , envModuleId = S.mkId "none"
  }

setModuleId :: M4.ModuleId -> Env -> Env
setModuleId i e = e { envModuleId = i }

addPos :: P.Pos -> Env -> Env
addPos p e = e { envPositions = p : envPositions e }

addIfdef :: Bool -> M4.IfdefId -> Env -> Env
addIfdef b i e = e { envConditions = (Left i, b) : envConditions e }

addCond :: Bool -> S.CondExpr -> Env -> Env
addCond b c e = e { envConditions = (Right c, b) : envConditions e }

type M a = ReaderT Env (State St) a

getUnique :: M Int
getUnique = do
  st <- get
  let n = unique_supply st
  put $ st { unique_supply = n + 1 }
  return n

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

addkeyMapSet :: (Ord k, Ord a) => k -> Map k (Set a) -> Map k (Set a)
addkeyMapSet = Map.alter (maybe (Just Set.empty) Just)

-- | Most class/permission pairs (c,p) indicate an information flow
-- between a *process* and an object of class c. However, some may
-- implicitly involve a different class: e.g. filesystem:associate
-- relates a *file* to a filesystem.
activeClass :: S.ClassId -> S.PermissionId -> S.ClassId
activeClass c p
  | c == S.mkId "filesystem" && p == S.mkId "associate" = S.mkId "file"
  | otherwise = processClassId

addAllow :: S.TypeOrAttributeId -> S.TypeOrAttributeId
         -> S.ClassId -> Set S.PermissionId -> M ()
addAllow subject object cls perms = do
  -- discard all but the outermost enclosing source position
  -- so that we only get the position of the top-level macro
  ps <- asks (Set.fromList . take 1 . reverse . envPositions)
  conds <- asks envConditions
  let rule = AllowRule subject object cls conds
  let posMap = Map.fromSet (const ps) perms
  let activeClasses = Set.map (activeClass cls) perms
  modify $ \st -> st
    { object_classes =
        Map.insertWith Set.union subject activeClasses $
        insertMapSet object cls $
        object_classes st
    , class_perms =
        flip (foldr (flip insertMapSet activePermissionId)) (Set.toList activeClasses) $
        Map.insertWith (flip Set.union) cls perms (class_perms st)
    , allow_rules = Map.insertWith (Map.unionWith Set.union) rule posMap (allow_rules st)
    }

addDeclaration :: S.IsIdentifier i => i -> M ()
addDeclaration i = do
  m <- asks envModuleId
  modify $ \st ->
    st { type_modules = Map.insert (S.toId i) m (type_modules st) }

addAttrib :: S.AttributeId -> M ()
addAttrib attr =
  modify $ \st -> st
    { attrib_members = addkeyMapSet attr (attrib_members st)
    }

addTypeAttrib :: S.TypeId -> S.AttributeId -> M ()
addTypeAttrib ty attr = modify f
  where
    f st = st
      { object_classes = addkeyMapSet (S.fromId (S.toId attr)) (object_classes st)
      , attrib_members = insertMapSet attr ty (attrib_members st)
      }

addTypeAttribs :: S.TypeId -> [S.AttributeId] -> M ()
addTypeAttribs ty attrs = mapM_ (addTypeAttrib ty) attrs

addTypeTransition :: S.TypeId -> S.TypeId -> S.ClassId -> S.TypeId -> M ()
addTypeTransition subj rel cls new = modify f
  where
    f st = st
      { object_classes =
          insertMapSet (S.fromId (S.toId subj)) processClassId $
          insertMapSet (S.fromId (S.toId new)) cls $
          object_classes st
      , type_transitions = Set.insert (subj, rel, cls, new) (type_transitions st)
      }

addDomtransMacro :: [S.TypeOrAttributeId] -> M ()
addDomtransMacro args = do
  n <- getUnique
  let i = S.mkId ("domtrans" ++ show n)
  addDeclaration i
  modify $ \ st -> st
      { object_classes =
          foldr ($) (object_classes st)
            [ Map.insertWith Set.union d cs | (d, cs) <- zip args argClasses ]
      , class_perms =
          insertMapSet (S.mkId "file") (S.mkId "x_file_perms") $
          class_perms st
      , domtrans_macros = Set.insert (i, args) (domtrans_macros st)
      }
  where
    argClasses :: [Set S.ClassId]
    argClasses =
      [ Set.fromList [processClassId, S.mkId "fd", S.mkId "fifo_file"]
      , Set.fromList [S.mkId "file"]
      , Set.fromList [processClassId]
      ]

processStmts :: M4.Stmts -> M ()
processStmts = mapM_ processStmt

processStmt :: M4.Stmt -> M ()
processStmt stmt =
  case stmt of
    Tunable c stmts1 stmts2 -> do
      local (addCond True c) $ processStmts stmts1
      local (addCond False c) $ processStmts stmts2
    Optional stmts1 stmts2 -> do
      processStmts stmts1
      processStmts stmts2
    Ifdef i stmts1 stmts2 -> do
      local (addIfdef True i) $ processStmts stmts1
      local (addIfdef False i) $ processStmts stmts2
    Ifndef i stmts -> local (addIfdef False i) $ processStmts stmts
    RefPolicyWarn {} -> return ()
    Call m4id [al, bl, cl] | m4id == S.mkId "domtrans_pattern" ->
      sequence_ $
        [ addDomtransMacro [a, b, c]
        | a <- map (S.fromId . S.toId) $ filterSignedId $ toList al
        , b <- map (S.fromId . S.toId) $ filterSignedId $ toList bl
        , c <- map (S.fromId . S.toId) $ filterSignedId $ toList cl
        ]
    Call _ _ -> return () -- FIXME
    Role {}           -> return () -- role
    AttributeRole {}  -> return () -- attribute_role
    RoleAttribute {}  -> return () -- roleattribute
    RoleTransition {} -> return ()
    RoleAllow {}      -> return ()
    Attribute attr -> addDeclaration attr >> addAttrib attr
    Type t _aliases attrs -> addDeclaration t >> addTypeAttribs t attrs -- TODO: track aliases
    TypeAlias _t _aliases -> return () -- TODO: track aliases
    TypeAttribute t attrs -> addTypeAttribs t (toList attrs)
    RangeTransition {} -> return ()
    TeNeverAllow {}    -> return () -- neverallow
    Transition trans (S.SourceTarget al bl cl) t ->
      case trans of
        S.TypeTransition ->
          sequence_ $
            [ addTypeTransition subject related classId object
            | let object = (S.fromId . S.toId) t
            , subject <- map (S.fromId . S.toId) $ filterSignedId $ toList al
            , related <- map (S.fromId . S.toId) $ filterSignedId $ toList bl
            , classId <- toList cl
            ]
        S.TypeMember -> return () -- type_member
        S.TypeChange -> return ()
    TeAvTab ad (S.SourceTarget al bl cl) ps ->
      case isAllow ad of
        True -> case ps of
          S.Permissions dl ->
            sequence_ $
              [ addAllow subject object classId perms
              | subject <- filterSignedId $ toList al
              , object' <- filterSignedId $ toList bl
              , let object = fromSelf subject object'
              , classId <- toList cl
              , let perms = Set.fromList $ toList dl
              ]
          S.PStarTilde st -> case st of
            S.Tilde _ -> return () -- FIXME
            S.Star    -> return () -- FIXME
        False -> return () -- dontaudit / auditdeny
    CondStmt c stmts1 stmts2 -> do
      local (addCond True c) $ processStmts stmts1
      local (addCond False c) $ processStmts stmts2
    XMLDocStmt {}        -> return ()
    SidStmt {}           -> return ()
    FileSystemUseStmt {} -> return ()
    GenFileSystemStmt {} -> return ()
    PortStmt {}          -> return ()
    NetInterfaceStmt {}  -> return ()
    NodeStmt {}          -> return ()
    Define {}            -> return ()
    Require {}           -> return () -- gen_require
    GenBoolean {}        -> return () -- gen_bool / gen_tunable
    StmtPosition stmt1 pos -> local (addPos pos) $ processStmt stmt1

isAllow :: S.AllowDeny -> Bool
isAllow ad = case ad of
  S.Allow      -> True
  S.AuditAllow -> True
  S.AuditDeny  -> False
  S.DontAudit  -> False

processImplementation :: M4.Implementation -> M ()
processImplementation (M4.Implementation modId _ stmts) =
  local (setModuleId modId) $ mapM_ processStmt stmts

processPolicyModule :: M4.PolicyModule -> M ()
processPolicyModule m = processImplementation (M4.implementation m)

processPolicy :: M4.Policy -> M ()
processPolicy policy = mapM_ processPolicyModule (M4.policyModules policy)

----------------------------------------------------------------------
-- Sub-attributes

-- TODO: read these in from a file
subattributes :: [(S.AttributeId, S.AttributeId)]
subattributes = [ (S.mkId a, S.mkId b) | (a, b) <- attrs ]
  where
    attrs =
      [ ("client_packet_type", "packet_type")
      , ("server_packet_type", "packet_type")
      , ("defined_port_type", "port_type")
      , ("reserved_port_type", "port_type")
      , ("unreserved_port_type", "port_type")
      , ("rpc_port_type", "reserved_port_type")
      , ("non_auth_file_type", "file_type")
      , ("non_security_file_type", "non_auth_file_type")
      , ("httpdcontent", "non_security_file_type")
      , ("lockfile", "non_security_file_type")
      , ("logfile", "non_security_file_type")
      , ("pidfile", "non_security_file_type")
      ]

-- | Requirement: isTopSorted subattributes
isTopSorted :: Eq a => [(a, a)] -> Bool
isTopSorted [] = True
isTopSorted (x : xs) = isTopSorted xs && snd x `notElem` map fst xs

-- | For each type t in attribute a, and class c used with a, note
-- that c is also used with t.
processAttributes :: M ()
processAttributes = do
  st <- get
  let new = Map.fromListWith Set.union
        [ (S.fromId (S.toId t), cs)
        | (attr, ts) <- Map.assocs (attrib_members st)
        , let cs = MapSet.lookup (S.fromId (S.toId attr)) (object_classes st)
        , t <- Set.toList ts ]
  put $ st { object_classes = Map.unionWith Set.union new (object_classes st) }

-- | Checking of sub-attribute membership: Ensure that all types t in
-- attribute x are also in attribute y.
checkSubAttribute :: (S.AttributeId, S.AttributeId) -> M Bool
checkSubAttribute (x, y) = do
  m <- gets attrib_members
  return (Set.isSubsetOf (MapSet.lookup x m) (MapSet.lookup y m))
  -- TODO: use error monad with a decent error message instead of returning Bool.

-- | For each pair (x, y) (indicating that x is a sub-attribute of y)
-- we 1) for any class c used with y, note that c is also used with x;
-- 2) drop the explicit membership of t in y for any type t in x.
processSubAttribute :: (S.AttributeId, S.AttributeId) -> M ()
processSubAttribute (x, y) = do
  st <- get
  let oc = object_classes st
  let oc' = Map.insertWith Set.union (S.fromId (S.toId x)) (MapSet.lookup (S.fromId (S.toId y)) oc) oc
  let am = attrib_members st
  let am' = Map.insertWith (flip Set.difference) y (MapSet.lookup x am) am
  put $ st { object_classes = oc', attrib_members = am' }

processSubAttributes :: [(S.AttributeId, S.AttributeId)] -> M Bool
processSubAttributes subs = do
  oks <- mapM checkSubAttribute subs
  mapM_ processSubAttribute subs
  return (isTopSorted subs && and oks)

----------------------------------------------------------------------
-- Generation of Lobster code

data OutputMode = Mode1 | Mode2 | Mode3 -- TODO: more sensible names
  deriving (Eq, Ord, Show)

type Dom = L.Name
type Port = L.Name

activePort :: Port
activePort = L.mkName "active"

memberPort :: Port
memberPort = L.mkName "member"

attributePort :: Port
attributePort = L.mkName "attribute"

toDom :: S.IsIdentifier i => i -> Dom
toDom = L.mkName . lowercase . S.idString

toPort :: S.IsIdentifier i => i -> Port
toPort = L.mkName . lowercase . S.idString

toIdentifier :: S.IsIdentifier i => i -> S.ClassId -> Dom
toIdentifier typeId classId = L.mkName (lowercase (S.idString typeId ++ "__" ++ S.idString classId))

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
    classDecl (classId, perms) = L.newClass (toClassId classId) [] (header ++ stmts)
      where
        header = map L.newPort [memberPort, attributePort, transitionPort]
        stmts = [ L.newPort (toPort p) | p <- Set.toList perms ]

    transitionPort :: L.Name
    transitionPort = L.mkName "type_transition"

    toClassId :: S.ClassId -> L.Name
    toClassId = L.mkName . capitalize . S.idString

outputModule :: M4.ModuleId -> L.ConnectAnnotation
outputModule i =
  L.mkAnnotation (L.mkName "Module")
    [L.annotationString (S.idString i)]

outputPerm :: S.PermissionId -> L.ConnectAnnotation
outputPerm p =
  L.mkAnnotation (L.mkName "Perm")
    [L.annotationString (S.idString p)]

outputPos :: P.Pos -> L.ConnectAnnotation
outputPos (P.Pos fname _ l c) =
  L.mkAnnotation (L.mkName "SourcePos")
    [L.annotationString fname, L.annotationInt l, L.annotationInt c]

outputCond :: (Either M4.IfdefId S.CondExpr, Bool) -> L.ConnectAnnotation
outputCond (Left i, b) =
  L.mkAnnotation (L.mkName (if b then "Ifdef" else "Ifndef"))
    [L.annotationString (S.idString i)]
outputCond (Right c, b) =
  L.mkAnnotation (L.mkName "CondExpr")
    [outputCondExpr (if b then c else S.Not c)]

outputCondExpr :: S.CondExpr -> C.Exp C.Span
outputCondExpr c =
  case c of
    S.Not c1      -> C.ExpUnaryOp C.emptySpan C.UnaryOpNot (outputCondExpr c1)
    S.Op c1 op c2 -> C.ExpBinaryOp C.emptySpan (outputCondExpr c1) (outputOp op) (outputCondExpr c2)
    S.Var i       -> C.ExpVar (L.mkName (S.idString i))
  where
    outputOp :: S.Op -> C.BinaryOp
    outputOp S.And = C.BinaryOpAnd
    outputOp S.Or  = C.BinaryOpOr
    outputOp S.Xor = error "xor unimplemented"
    outputOp S.Equals = C.BinaryOpEqual
    outputOp S.Notequal = C.BinaryOpNotEqual

-- | Mode 1
outputAllowRule1 :: AllowRule -> (S.PermissionId, Set P.Pos) -> L.Decl
outputAllowRule1 (AllowRule subject object cls conds) (perm, ps) =
  L.neutral'
    (L.domPort (toIdentifier subject (activeClass cls perm)) activePort)
    (L.domPort (toIdentifier object cls) (toPort perm))
    (map outputCond conds ++ map outputPos (Set.toList ps))

outputAllowRule2 :: (AllowRule, Map S.PermissionId (Set P.Pos)) -> L.Decl
outputAllowRule2 (AllowRule subject object cls conds, m) =
  L.neutral'
    (L.domPort (toDom subject) activePort)
    (L.domPort (toDom object) (toPort cls))
    (map outputPerm perms ++ map outputCond conds ++ map outputPos ps)
  where
    perms = Map.keys m
    ps = Set.toList (Set.unions (Map.elems m))

-- | If both ends of the edge are located in the same module, then we
-- connect them directly. Otherwise, we split either or both ends into
-- separate edges: The main edge connects to the "ext" port of the
-- module domain, and encodes its final destination in an annotation.
-- Then a secondary edge connects the internal port to the "ext" port
-- of its module.
moduleEdges ::
  St -> (S.Identifier, Port) -> (S.Identifier, Port) -> [L.ConnectAnnotation] -> [(Maybe M4.ModuleId, L.Decl)]
moduleEdges st (d1, p1) (d2, p2) anns
  | m1 == m2 = [(m1, L.neutral' (L.domPort (toDom d1) p1) (L.domPort (toDom d2) p2) anns)]
  | otherwise = (Nothing, L.neutral' dp1' dp2' (a1 ++ a2 ++ anns)) : e1 ++ e2
  where
    m1 = Map.lookup d1 (type_modules st)
    m2 = Map.lookup d2 (type_modules st)
    ext m = L.domPort (toDom m) (L.mkName "ext")
    dp1 = L.domPort (toDom d1) p1
    dp2 = L.domPort (toDom d2) p2
    (dp1', a1, e1) = case m1 of
      Nothing -> (dp1, [], [])
      Just m -> ( ext m
                , [L.mkAnnotation (L.mkName "Lhs") [L.annotationName (toDom d1), L.annotationName p1]]
                , [(Just m, L.neutral dp1 (L.extPort (L.mkName "ext")))])
    (dp2', a2, e2) = case m2 of
      Nothing -> (dp2, [], [])
      Just m -> ( ext m
                , [L.mkAnnotation (L.mkName "Rhs") [L.annotationName (toDom d2), L.annotationName p2]]
                , [(Just m, L.neutral dp2 (L.extPort (L.mkName "ext")))])

outputAllowRule3 :: St -> (AllowRule, Map S.PermissionId (Set P.Pos)) -> [(Maybe M4.ModuleId, L.Decl)]
outputAllowRule3 st (AllowRule subject object cls conds, m) =
  moduleEdges st
    (S.toId subject, activePort)
    (S.toId object, toPort cls)
    (map outputPerm perms ++ map outputCond conds ++ map outputPos ps)
  where
    perms = Map.keys m
    ps = Set.toList (Set.unions (Map.elems m))

outputAllowRules1 :: (AllowRule, Map S.PermissionId (Set P.Pos)) -> [L.Decl]
outputAllowRules1 (rule, m) = map (outputAllowRule1 rule) (Map.assocs m)

outputAttribute1 :: S.TypeOrAttributeId -> S.TypeOrAttributeId -> S.ClassId -> L.Decl
outputAttribute1 ty attr cls =
  L.neutral'
    (L.domPort (toIdentifier ty cls) memberPort)
    (L.domPort (toIdentifier attr cls) attributePort)
    [L.mkAnnotation (L.mkName "Attribute") []]

outputAttributes1 :: St -> S.TypeOrAttributeId -> S.TypeOrAttributeId -> [L.Decl]
outputAttributes1 st ty attr = [ outputAttribute1 ty attr cls | cls <- classes ]
  where
    classesOf t = Map.findWithDefault Set.empty t (object_classes st)
    classes = Set.toList $ Set.intersection (classesOf ty) (classesOf attr)
-- FIXME: Don't do this intersection. We should output the attribute
-- membership edge at all classes for which the attribute has allow
-- rules (i.e. 'classesOf attr'). We must separately ensure that the
-- type is instantiated for (at least) those classes.

outputSubAttribute1 :: S.AttributeId -> S.AttributeId -> S.ClassId -> L.Decl
outputSubAttribute1 sub sup cls =
  L.neutral'
    (L.domPort (toIdentifier sub cls) memberPort)
    (L.domPort (toIdentifier sup cls) attributePort)
    [L.mkAnnotation (L.mkName "SubAttribute") []]

outputSubAttributes1 :: St -> (S.AttributeId, S.AttributeId) -> [L.Decl]
outputSubAttributes1 st (sub, sup) = [ outputSubAttribute1 sub sup cls | cls <- classes ]
  where
    classesOf t = Map.findWithDefault Set.empty (S.fromId (S.toId t)) (object_classes st)
    classes = Set.toList $ Set.intersection (classesOf sub) (classesOf sup)

outputAttribute2 :: S.TypeId -> S.AttributeId -> L.Decl
outputAttribute2 ty attr =
  L.neutral'
    (L.domPort (toDom ty) memberPort)
    (L.domPort (toDom attr) attributePort)
    [L.mkAnnotation (L.mkName "Attribute") []]

outputAttribute3 :: St -> S.TypeId -> S.AttributeId -> [(Maybe M4.ModuleId, L.Decl)]
outputAttribute3 st ty attr =
  moduleEdges st
    (S.toId ty, memberPort)
    (S.toId attr, attributePort)
    [L.mkAnnotation (L.mkName "Attribute") []]

outputSubAttribute2 :: S.AttributeId -> S.AttributeId -> L.Decl
outputSubAttribute2 ty attr =
  L.neutral'
    (L.domPort (toDom ty) memberPort)
    (L.domPort (toDom attr) attributePort)
    [L.mkAnnotation (L.mkName "SubAttribute") []]

outputSubAttribute3 :: St -> S.AttributeId -> S.AttributeId -> [(Maybe M4.ModuleId, L.Decl)]
outputSubAttribute3 st ty attr =
  moduleEdges st
    (S.toId ty, memberPort)
    (S.toId attr, attributePort)
    [L.mkAnnotation (L.mkName "SubAttribute") []]

outputTypeTransition1 :: (S.TypeId, S.TypeId, S.ClassId, S.TypeId) -> L.Decl
outputTypeTransition1 (subj, rel, cls, new) =
  L.neutral'
    (L.domPort (toIdentifier subj processClassId) activePort)
    (L.domPort (toIdentifier new cls) transitionPort)
    [L.mkAnnotation (L.mkName "TypeTransition") [L.annotationString (S.idString rel)]]
  where
    transitionPort :: L.Name
    transitionPort = L.mkName "type_transition"

outputTypeTransition2 :: (S.TypeId, S.TypeId, S.ClassId, S.TypeId) -> L.Decl
outputTypeTransition2 (subj, rel, cls, new) =
  L.neutral'
    (L.domPort (toDom subj) activePort)
    (L.domPort (toDom new) (toPort cls))
    [L.mkAnnotation (L.mkName "TypeTransition") [L.annotationString (S.idString rel)]]

outputTypeTransition3 :: St -> (S.TypeId, S.TypeId, S.ClassId, S.TypeId) -> [(Maybe M4.ModuleId, L.Decl)]
outputTypeTransition3 st (subj, rel, cls, new) =
  moduleEdges st
    (S.toId subj, activePort)
    (S.toId new, toPort cls)
    [L.mkAnnotation (L.mkName "TypeTransition") [L.annotationString (S.idString rel)]]

outputDomtransMacro1 :: (S.Identifier, [S.TypeOrAttributeId]) -> [L.Decl]
outputDomtransMacro1 (n, ds) = domDecl : map connectArg args
  where
    d :: L.Name
    d = L.mkName (S.idString n)

    domDecl :: L.Decl
    domDecl = L.newDomain' d (L.mkName "Domtrans_pattern") [L.mkName (show (S.idString (ds !! 1)))]
      [L.mkAnnotation (L.mkName "Macro") (map (L.annotationString . S.idString) ds)]

    connectArg :: (Int, S.ClassId, S.PermissionId, String) -> L.Decl
    connectArg (i, cls, perm, argname) =
      L.neutral'
        (L.domPort (toIdentifier (ds !! i) cls) (toPort perm))
        (L.domPort d (L.mkName argname))
        [L.mkAnnotation (L.mkName "MacroArg") []]

    args :: [(Int, S.ClassId, S.PermissionId, String)]
    args =
      [ (0, processClassId    , activePermissionId         , "d1_active"         )
      , (0, S.mkId "fd"       , S.mkId "use"               , "d1_fd_use"         )
      , (0, S.mkId "fifo_file", S.mkId "rw_fifo_file_perms", "d1_fifo"           )
      , (0, processClassId    , S.mkId "sigchld"           , "d1_sigchld"        )
      , (1, S.mkId "file"     , S.mkId "x_file_perms"      , "d2"                )
      , (2, processClassId    , activePermissionId         , "d3_active"         )
      , (2, processClassId    , S.mkId "transition"        , "d3_transition"     )
      , (2, processClassId    , S.mkId "type_transition"   , "d3_type_transition")
      ]

outputDomtransMacro2 :: (S.Identifier, [S.TypeOrAttributeId]) -> [L.Decl]
outputDomtransMacro2 (n, ds) = domDecl : map connectArg args
  where
    d :: Dom
    d = L.mkName (S.idString n)

    domDecl :: L.Decl
    domDecl = L.newDomain' d (L.mkName "Domtrans_pattern") [L.mkName (show (S.idString (ds !! 1)))]
      [L.mkAnnotation (L.mkName "Macro") (map (L.annotationString . S.idString) ds)]

    connectArg :: (Int, Port, String) -> L.Decl
    connectArg (i, port, argname) =
      L.neutral'
        (L.domPort (toDom (ds !! i)) port)
        (L.domPort d (L.mkName argname))
        [L.mkAnnotation (L.mkName "MacroArg") []]

    args :: [(Int, Port, String)]
    args =
      [ (0, activePort          , "d1_active"   )
      , (0, L.mkName "fd"       , "d1_fd"       )
      , (0, L.mkName "fifo_file", "d1_fifo_file")
      , (0, L.mkName "process"  , "d1_process"  )
      , (1, L.mkName "file"     , "d2_file"     )
      , (2, activePort          , "d3_active"   )
      , (2, L.mkName "process"  , "d3_process"  )
      ]

outputDomtransMacro3 :: St -> (S.Identifier, [S.TypeOrAttributeId]) -> [(Maybe M4.ModuleId, L.Decl)]
outputDomtransMacro3 st (n, ds) = (m, domDecl) : concatMap connectArg args
  where
    d :: Dom
    d = L.mkName (S.idString n)

    m :: Maybe M4.ModuleId
    m = Map.lookup n (type_modules st)

    domDecl :: L.Decl
    domDecl = L.newDomain' d (L.mkName "Domtrans_pattern") [L.mkName (show (S.idString (ds !! 1)))]
      [L.mkAnnotation (L.mkName "Macro") (map (L.annotationString . S.idString) ds)]

    connectArg :: (Int, Port, String) -> [(Maybe M4.ModuleId, L.Decl)]
    connectArg (i, port, argname) =
      moduleEdges st
        (S.toId (ds !! i), port)
        (S.mkId (L.nameString d), L.mkName argname)
        [L.mkAnnotation (L.mkName "MacroArg") []]

    args :: [(Int, Port, String)]
    args =
      [ (0, activePort          , "d1_active"   )
      , (0, L.mkName "fd"       , "d1_fd"       )
      , (0, L.mkName "fifo_file", "d1_fifo_file")
      , (0, L.mkName "process"  , "d1_process"  )
      , (1, L.mkName "file"     , "d2_file"     )
      , (2, activePort          , "d3_active"   )
      , (2, L.mkName "process"  , "d3_process"  )
      ]

domtransDecl1 :: L.Decl
domtransDecl1 =
  L.newClass (L.mkName "Domtrans_pattern") [d2_name]
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
    , L.neutral' (L.extPort d1_active) (L.extPort d3_tt)
        [L.mkAnnotation (L.mkName "TypeTransition") [L.annotationName d2_name]]

    , L.neutral (L.extPort d3_active) (L.extPort d1_fd_use)
    , L.neutral (L.extPort d3_active) (L.extPort d1_fifo)
    , L.neutral (L.extPort d3_active) (L.extPort d1_sig)
    ]
  where
    d1_active = L.mkName "d1_active"
    d1_fd_use = L.mkName "d1_fd_use"
    d1_fifo   = L.mkName "d1_fifo"
    d1_sig    = L.mkName "d1_sigchld"
    d2        = L.mkName "d2"
    d3_active = L.mkName "d3_active"
    d3_trans  = L.mkName "d3_transition"
    d3_tt     = L.mkName "d3_type_transition"
    d2_name   = L.mkName "d2_name"

domtransDecl2 :: L.Decl
domtransDecl2 =
  L.newClass (L.mkName "Domtrans_pattern") [d2_name]
    [ L.newPort d1_active
    , L.newPort d1_fd
    , L.newPort d1_fifo
    , L.newPort d1_proc
    , L.newPort d2_file
    , L.newPort d3_active
    , L.newPort d3_proc
    , L.neutral' (L.extPort d1_active) (L.extPort d2_file)
        [outputPerm (S.mkId "x_file_perms")]
    , L.neutral' (L.extPort d1_active) (L.extPort d3_proc)
        [outputPerm (S.mkId "transition"),
         L.mkAnnotation (L.mkName "TypeTransition") [L.annotationName d2_name]]
    , L.neutral' (L.extPort d3_active) (L.extPort d1_fd)
        [outputPerm (S.mkId "use")]
    , L.neutral' (L.extPort d3_active) (L.extPort d1_fifo)
        [outputPerm (S.mkId "rw_fifo_file_perms")]
    , L.neutral' (L.extPort d3_active) (L.extPort d1_proc)
        [outputPerm (S.mkId "sigchld")]
    ]
  where
    d1_active = L.mkName "d1_active"
    d1_fd     = L.mkName "d1_fd"
    d1_fifo   = L.mkName "d1_fifo_file"
    d1_proc   = L.mkName "d1_process"
    d2_file   = L.mkName "d2_file"
    d3_active = L.mkName "d3_active"
    d3_proc   = L.mkName "d3_process"
    d2_name   = L.mkName "d2_name"

outputLobster1 :: M4.Policy -> St -> [L.Decl]
outputLobster1 policy st =
  domtransDecl1 :
  classDecls policy st ++ domainDecls ++ connectionDecls ++ attributeDecls ++ subAttributeDecls
    ++ transitionDecls ++ domtransDecls
  where
    toClassId :: S.ClassId -> L.Name
    toClassId = L.mkName . capitalize . S.idString

    domainDecls :: [L.Decl]
    domainDecls =
      [ L.newDomain' (toIdentifier typeId classId) (toClassId classId) [] [ann1, ann2]
      | (typeId, classIds) <- Map.assocs (object_classes st)
      , classId <- Set.toList classIds
      , let ann1 =
              if Map.member (S.fromId (S.toId typeId)) (attrib_members st)
                then L.mkAnnotation (L.mkName "Attribute") []
                else L.mkAnnotation (L.mkName "Type") []
      , let modId =
              case Map.lookup (S.toId typeId) (type_modules st) of
                Just m -> m
                Nothing -> S.mkId "unknown"
      , let ann2 = outputModule modId
      ]

    connectionDecls :: [L.Decl]
    connectionDecls = concatMap outputAllowRules1 (Map.assocs (allow_rules st))

    attributeDecls :: [L.Decl]
    attributeDecls = do
      (attr, tys) <- Map.assocs (attrib_members st)
      ty <- Set.toList tys
      outputAttributes1 st (S.fromId (S.toId ty)) (S.fromId (S.toId attr))

    subAttributeDecls :: [L.Decl]
    subAttributeDecls = concatMap (outputSubAttributes1 st) subattributes

    transitionDecls :: [L.Decl]
    transitionDecls = map outputTypeTransition1 (Set.toList (type_transitions st))

    domtransDecls :: [L.Decl]
    domtransDecls = concatMap outputDomtransMacro1 (Set.toList (domtrans_macros st))

outputLobster2 :: St -> [L.Decl]
outputLobster2 st =
  domtransDecl2 :
  domainDecls ++ connectionDecls ++ attributeDecls ++ subAttributeDecls
    ++ transitionDecls ++ domtransDecls
  where
    domainDecl :: (S.TypeOrAttributeId, Set S.ClassId) -> L.Decl
    domainDecl (ty, classes) =
      L.anonDomain' (toDom ty) (header ++ stmts) [ann1, ann2]
      where
        header = map L.newPort [activePort, memberPort, attributePort]
        stmts = [ L.newPort (toPort c) | c <- Set.toList classes ]
        ann1 =
          if Map.member (S.fromId (S.toId ty)) (attrib_members st)
            then L.mkAnnotation (L.mkName "Attribute") []
            else L.mkAnnotation (L.mkName "Type") []
        modId =
          case Map.lookup (S.toId ty) (type_modules st) of
            Just m -> m
            Nothing -> S.mkId "unknown"
        ann2 = outputModule modId

    domainDecls :: [L.Decl]
    domainDecls = map domainDecl (Map.assocs (object_classes st))

    connectionDecls :: [L.Decl]
    connectionDecls = map outputAllowRule2 (Map.assocs (allow_rules st))

    attributeDecls :: [L.Decl]
    attributeDecls = do
      (attr, tys) <- Map.assocs (attrib_members st)
      [ outputAttribute2 ty attr | ty <- Set.toList tys ]

    subAttributeDecls :: [L.Decl]
    subAttributeDecls =
      [ outputSubAttribute2 sub sup | (sub, sup) <- subattributes ]

    transitionDecls :: [L.Decl]
    transitionDecls = map outputTypeTransition2 (Set.toList (type_transitions st))

    domtransDecls :: [L.Decl]
    domtransDecls = concatMap outputDomtransMacro2 (Set.toList (domtrans_macros st))

outputLobster3 :: St -> [L.Decl]
outputLobster3 st =
  domtransDecl2 :
  [ L.anonDomain (toDom m) (L.newPort (L.mkName "ext") : reverse ds)
    | (Just m, ds) <- Map.assocs groupedDecls ] ++
  Map.findWithDefault [] Nothing groupedDecls
  where
    domainDecl :: (S.TypeOrAttributeId, Set S.ClassId) -> (Maybe M4.ModuleId, L.Decl)
    domainDecl (ty, classes) =
      (Map.lookup (S.toId ty) (type_modules st),
       L.anonDomain' (toDom ty) (header ++ stmts) [ann])
      where
        header = map L.newPort [activePort, memberPort, attributePort]
        stmts = [ L.newPort (toPort c) | c <- Set.toList classes ]
        ann =
          if Map.member (S.fromId (S.toId ty)) (attrib_members st)
            then L.mkAnnotation (L.mkName "Attribute") []
            else L.mkAnnotation (L.mkName "Type") []

    domainDecls :: [(Maybe M4.ModuleId, L.Decl)]
    domainDecls = map domainDecl (Map.assocs (object_classes st))

    connectionDecls :: [(Maybe M4.ModuleId, L.Decl)]
    connectionDecls = concatMap (outputAllowRule3 st) (Map.assocs (allow_rules st))

    attributeDecls :: [(Maybe M4.ModuleId, L.Decl)]
    attributeDecls = do
      (attr, tys) <- Map.assocs (attrib_members st)
      ty <- Set.toList tys
      outputAttribute3 st ty attr

    subAttributeDecls :: [(Maybe M4.ModuleId, L.Decl)]
    subAttributeDecls = do
      (sub, sup) <- subattributes
      outputSubAttribute3 st sub sup

    transitionDecls :: [(Maybe M4.ModuleId, L.Decl)]
    transitionDecls = do
      tt <- Set.toList (type_transitions st)
      outputTypeTransition3 st tt

    domtransDecls :: [(Maybe M4.ModuleId, L.Decl)]
    domtransDecls = concatMap (outputDomtransMacro3 st) (Set.toList (domtrans_macros st))

    taggedDecls :: [(Maybe M4.ModuleId, L.Decl)]
    taggedDecls =
      domainDecls ++ connectionDecls ++ attributeDecls ++ subAttributeDecls
        ++ transitionDecls ++ domtransDecls

    groupedDecls :: Map (Maybe M4.ModuleId) [L.Decl] -- in reverse order
    groupedDecls = Map.fromListWith (++) [ (m, [d]) | (m, d) <- taggedDecls ]

outputLobster :: OutputMode -> M4.Policy -> St -> [L.Decl]
outputLobster Mode1 policy = outputLobster1 policy
outputLobster Mode2 _ = outputLobster2
outputLobster Mode3 _ = outputLobster3

----------------------------------------------------------------------

-- | Convert a policy to Lobster.
toLobster :: OutputMode -> Policy -> Either Error [L.Decl]
toLobster mode policy0 = do
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
  let action = processPolicy policy >> processAttributes >> processSubAttributes subattributes
  let (ok, finalSt) = runState (runReaderT action initEnv) initSt
  if ok
    then return (outputLobster mode policy finalSt)
    else Left (Error "subattribute check failed")

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

lowercase :: String -> String
lowercase "domain" = "domain_type" -- FIXME: ugly hack
lowercase "" = ""
lowercase (x:xs) = toLower x : xs
