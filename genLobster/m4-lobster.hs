{-# OPTIONS -Wall #-}
module Main (main) where

import Control.Monad.State.Strict
import Data.Char
import Data.Either (partitionEithers)
import Data.Foldable (Foldable, toList)
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
  { object_classes :: !(Map S.TypeOrAttributeId (Set S.ClassId))
  , class_perms    :: !(Map S.ClassId (Set S.PermissionId))
  , allow_rules    :: !(Set AllowRule)
  }

processClassId :: S.ClassId
processClassId = S.mkId "process"

activePermissionId :: S.PermissionId
activePermissionId = S.mkId "active"

initSt :: St
initSt = St
  { object_classes = Map.empty
  , class_perms    = Map.singleton processClassId (Set.singleton activePermissionId)
  , allow_rules    = Set.empty
  }

type M a = State St a

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
addAllow subject object cls perms = modify f
  where
    f st = St
      { object_classes =
          insertMapSet subject processClassId $
          insertMapSet object cls $
          object_classes st
      , class_perms = Map.insertWith (flip Set.union) cls perms (class_perms st)
      , allow_rules = foldr (Set.insert . AllowRule subject object cls)
                            (allow_rules st) (Set.toList perms)
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
    TeAvTab S.Allow (S.SourceTarget al bl cl) (S.Permissions dl) ->
      sequence_ $
        [ addAllow subject object classId perms
        | subject <- filterSignedId $ toList al
        , object' <- filterSignedId $ toList bl
        , let object = fromSelf subject object'
        , classId <- toList cl
        , let perms = Set.fromList $ toList dl
        ]
    _ -> return ()

processPolicy :: M4.Policy -> M ()
processPolicy policy = processStmts allStmts
  where
    allStmts :: M4.Stmts
    allStmts =
        [ stmt
        | m <- M4.policyModules policy
        , let M4.Implementation _ _ stmts = M4.implementation m
        , stmt <- stmts ]

----------------------------------------------------------------------
-- Generation of Lobster code

classDecls :: St -> [L.Decl]
classDecls st = map classDecl (Map.assocs (class_perms st))
  where
    classDecl :: (S.ClassId, Set S.PermissionId) -> L.Decl
    classDecl (classId, perms) = L.Class (toClassId classId) [] stmts
      where stmts = [ L.newPort (toPortId p) | p <- Set.toList perms ]

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

outputLobster :: St -> [L.Decl]
outputLobster st = classDecls st ++ domainDecls ++ connectionDecls
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
    connectionDecls = map outputAllowRule (Set.toList (allow_rules st))

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
  let macros = Map.unions [patternMacros, interfaceMacros, templateMacros]
  let policy = policy0 { policyModules = map (expandPolicyModule macros) (policyModules policy0) }
  let finalSt = execState (processPolicy policy) initSt
  putStrLn $ showLobster (outputLobster finalSt)

----------------------------------------------------------------------
-- Selective macro expansion

type Macros = Map M4Id [Stmt]

expandPolicyModule :: Macros -> PolicyModule -> PolicyModule
expandPolicyModule s pm =
  pm { interface = expandInterface s (interface pm)
     , implementation = expandImplementation s (implementation pm)
     }

expandImplementation :: Macros -> Implementation -> Implementation
expandImplementation s (Implementation i v stmts) = Implementation i v (expandStmts s stmts)

expandInterface :: Macros -> Interface -> Interface
expandInterface s (InterfaceModule doc es) = InterfaceModule doc (map (expandInterfaceElement s) es)

expandInterfaceElement :: Macros -> InterfaceElement -> InterfaceElement
expandInterfaceElement s (InterfaceElement ty doc i stmts) =
  InterfaceElement ty doc i (expandStmts s stmts)

expandStmts :: Macros -> Stmts -> Stmts
expandStmts s stmts = concatMap (expandStmt s) stmts

expandStmt :: Macros -> Stmt -> [Stmt]
expandStmt s stmt =
  case stmt of
    Tunable cond stmts1 stmts2  -> [Tunable cond (expandStmts s stmts1) (expandStmts s stmts2)]
    Optional stmts1 stmts2      -> [Optional (expandStmts s stmts1) (expandStmts s stmts2)]
    Ifdef i stmts1 stmts2       -> [Ifdef i (expandStmts s stmts1) (expandStmts s stmts2)]
    Ifndef i stmts              -> [Ifndef i (expandStmts s stmts)]
    CondStmt cond stmts1 stmts2 -> [CondStmt cond (expandStmts s stmts1) (expandStmts s stmts2)]
    Call i args -> -- args :: [NonEmptyList (SignedId Identifier)]
      case Map.lookup i s of
        Nothing -> [stmt]
        Just stmts -> expandStmts s $ map (substStmt args) stmts
    _ -> [stmt]

substStmt :: [NonEmptyList (S.SignedId S.Identifier)] -> Stmt -> Stmt
substStmt arguments = goStmt
  where
    goStmt :: Stmt -> Stmt
    goStmt stmt =
      case stmt of
        Tunable c stmts1 stmts2 -> Tunable (goCondExpr c) (map goStmt stmts1) (map goStmt stmts2)
        Optional stmts1 stmts2  -> Optional (map goStmt stmts1) (map goStmt stmts2)
        Ifdef i stmts1 stmts2   -> Ifdef i (map goStmt stmts1) (map goStmt stmts2)
        Ifndef i stmts          -> Ifndef i (map goStmt stmts)
        Call i args             -> Call i (map goSignedIds args)
        Role i attrs            -> Role (goId i) (goListSignedId attrs)
        RoleAttribute i attrs   -> RoleAttribute (goId i) (goIds attrs)
        RoleTransition rs ts i  -> RoleTransition (goIds rs) (goSignedIds ts) (goId i)
        RoleAllow rs1 rs2       -> RoleAllow (goIds rs1) (goIds rs2)
        Attribute a             -> Attribute (goId a)
        Type t ts attrs         -> Type (goId t) (goListId ts) (goListId attrs)
        TypeAlias t ts          -> TypeAlias (goId t) (goIds ts)
        TypeAttribute i attrs   -> TypeAttribute (goId i) (goIds attrs)
        RangeTransition xs ys zs (MlsRange a b)
                                -> RangeTransition (goSignedIds xs) (goSignedIds ys) (goIds zs)
                                   (MlsRange (id a) (id b))
        Transition t (S.SourceTarget st tt tc) i
                                -> Transition t (S.SourceTarget
                                                      (goSignedIds st)
                                                      (goSignedIds tt)
                                                      (goIds tc))
                                   (goId i)
        TeAvTab a (S.SourceTarget st tt tc) perms
                                -> TeAvTab a (S.SourceTarget
                                              (goSignedIds st)
                                              (substTargetTypes tt)
                                              (goIds tc))
                                   (goPermissions perms)
        CondStmt c ss1 ss2      -> CondStmt c (map goStmt ss1) (map goStmt ss2)
        GenBoolean t i b        -> GenBoolean t (goId i) b
        _ -> stmt

    goCondExpr :: S.CondExpr -> S.CondExpr
    goCondExpr (S.Not c) = S.Not (goCondExpr c)
    goCondExpr (S.Op c1 o c2) = S.Op (goCondExpr c1) o (goCondExpr c2)
    goCondExpr (S.Var i) = S.Var (goId i)

    goSignedIds :: S.IsIdentifier i => NonEmptyList (S.SignedId i) -> NonEmptyList (S.SignedId i)
    goSignedIds ids = fromList (substSignedId =<< toList ids)

    substTargetTypes :: NonEmptyList (S.SignedId S.Self) -> NonEmptyList (S.SignedId S.Self)
    substTargetTypes tt = fromList (substSignedIdSelf =<< toList tt)

    goId :: S.IsIdentifier i => i -> i
    goId i = fromSingle (substId' i)

    goListId :: S.IsIdentifier i => [i] -> [i]
    goListId ids = substId' =<< ids

    goIds :: S.IsIdentifier i => NonEmptyList i -> NonEmptyList i
    goIds ids = fromList (substId' =<< toList ids)

    goListSignedId :: S.IsIdentifier i => [S.SignedId i] -> [S.SignedId i]
    goListSignedId ids = substSignedId =<< ids

    substSignedId :: S.IsIdentifier i => S.SignedId i -> [S.SignedId i]
    substSignedId (S.SignedId S.Positive i) = substId i
    substSignedId (S.SignedId S.Negative i) = fmap negateSignedId (substId i)

    substSignedIdSelf :: S.SignedId S.Self -> [S.SignedId S.Self]
    substSignedIdSelf (S.SignedId S.Positive self) = substSelf self
    substSignedIdSelf (S.SignedId S.Negative self) = fmap negateSignedId (substSelf self)

    substSelf :: S.Self -> [S.SignedId S.Self]
    substSelf S.Self = return (S.SignedId S.Positive S.Self)
    substSelf (S.NotSelf i) = fmap (fmap S.NotSelf) (substId i)

    getArg :: Int -> [S.SignedId S.Identifier]
    getArg n | n <= length arguments = toList (arguments !! (n - 1))
             | otherwise             = []

    substId :: S.IsIdentifier i => i -> [S.SignedId i]
    substId i =
      case asDollar (S.idString i) of
        Nothing -> return (S.SignedId S.Positive (S.mkId (goString (S.idString i))))
        Just n -> fmap (fmap S.fromId) (getArg n)

    goString :: String -> String
    goString ('$':cs) = S.idString (fromPositive (fromSingle (getArg (read ds)))) ++ goString cs'
      where (ds, cs') = span isDigit cs
    goString (c : cs) = c : goString cs
    goString [] = []

    asDollar :: String -> Maybe Int
    asDollar ('$' : s)
      | all isDigit s = Just (read s)
    asDollar _ = Nothing

    substId' :: S.IsIdentifier i => i -> [i]
    substId' i = fmap fromPositive (substId i)

    fromPositive :: S.SignedId a -> a
    fromPositive (S.SignedId S.Positive x) = x
    fromPositive (S.SignedId S.Negative _) = error "fromPositive"

    fromSingle :: (Foldable l, Show a) => l a -> a
    fromSingle ys =
      case toList ys of
        [y] -> y
        _ -> error $ "fromSingle " ++ show (toList ys)

    negateSignedId :: S.SignedId a -> S.SignedId a
    negateSignedId (S.SignedId S.Positive x) = S.SignedId S.Negative x
    negateSignedId (S.SignedId S.Negative _) = error "negateSignedId Negative"

    goPermissions :: S.Permissions -> S.Permissions
    goPermissions (S.Permissions pids) = S.Permissions (goIds pids)
    goPermissions (S.PStarTilde S.Star) = S.PStarTilde S.Star
    goPermissions (S.PStarTilde (S.Tilde pids)) = S.PStarTilde (S.Tilde (goIds pids))

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
