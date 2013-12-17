--
-- selinux-lobster.hs --- Converting from SELinux .conf format to Lobster .lsr format.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Control.Monad.State.Strict

import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Char (toLower, toUpper)

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)

import Data.NonEmptyList (NonEmptyList)

import qualified SCD.Lobster.Gen.CoreSyn as L
import SCD.Lobster.Gen.CoreSyn.Output (showLobster)

import SCD.SELinux.Syntax
import SCD.SELinux.Parser (parsePolicy)

data AllowRule = AllowRule
  { allowSubject    :: TypeOrAttributeId
  , allowObject     :: TypeOrAttributeId
  , allowClass      :: ClassId
  , allowPerm       :: PermissionId
  } deriving (Eq, Ord, Show)

data St = St
  { actors         :: !(Set TypeOrAttributeId)
  , all_types      :: !(Set TypeOrAttributeId)
  , object_classes :: !(Map TypeOrAttributeId (Set ClassId))
  , object_perms   :: !(Map TypeOrAttributeId (Set (ClassId, PermissionId)))
  , allow_rules    :: !(Set AllowRule)
  }

initSt :: St
initSt = St
  { actors         = Set.empty
  , all_types      = Set.empty
  , object_classes = Map.empty
  , object_perms   = Map.empty
  , allow_rules    = Set.empty
  }

type M a = State St a

filterSignedId :: Eq a => [SignedId a] -> [a]
filterSignedId xs = [ y | y <- ys, y `notElem` zs ]
  where
    (ys, zs) = partitionEithers (map f xs)
    f (SignedId Positive y) = Left y
    f (SignedId Negative z) = Right z

fromSelf :: TypeOrAttributeId -> Self -> TypeOrAttributeId
fromSelf x Self = x
fromSelf _ (NotSelf x) = x

insertMapSet :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
insertMapSet k x = Map.insertWith (flip Set.union) k (Set.singleton x)

addAllow :: TypeOrAttributeId -> TypeOrAttributeId
         -> ClassId -> Set PermissionId -> M ()
addAllow subject object cls perms = modify f
  where
    perms' = Set.fromList [ (cls, p) | p <- Set.toList perms ]
    f st = St
      { actors = Set.insert subject (actors st)
      , all_types = Set.insert subject (Set.insert object (all_types st))
      , object_classes = insertMapSet object cls (object_classes st)
      , object_perms = Map.insertWith (flip Set.union) object perms' (object_perms st)
      , allow_rules = foldr (Set.insert . AllowRule subject object cls)
                            (allow_rules st) (Set.toList perms)
      }

processStmt :: Stmt -> M ()
processStmt stmt =
  case stmt of
    TeAvTab Allow (SourceTarget al bl cl) (Permissions dl) ->
      sequence_ $
        [ addAllow subject object classId perms
        | subject <- filterSignedId $ toList al
        , object' <- filterSignedId $ toList bl
        , let object = fromSelf subject object'
        , classId <- toList cl
        , let perms = Set.fromList $ toList dl
        ]
    _ -> return ()

processTeRbac :: TeRbac -> M ()
processTeRbac te =
  case te of
    -- Attribute AttributeId
    -- Type TypeId [TypeId] [AttributeId]
    -- TypeAlias TypeId (NonEmptyList TypeId)
    -- TypeAttribute TypeId (NonEmptyList AttributeId)
    -- BoolDef BoolId Bool
    -- TeNeverAllow (SourceTarget (NeverAllow TypeOrAttributeId) (NeverAllow Self)) Permissions
    -- Role RoleId [SignedId TypeOrAttributeId]
    -- Dominance (NonEmptyList (Tree RoleId))
    -- RoleTransition (NonEmptyList RoleId) (NonEmptyList (SignedId TypeOrAttributeId)) RoleId
    -- RoleAllow (NonEmptyList RoleId) (NonEmptyList RoleId)
    -- CondStmt CondExpr [RequireStmt] [RequireStmt]
    Stmt stmt -> processStmt stmt
    -- Optional AvRuleBlock (Maybe AvRuleBlock)
    _ -> return ()

processPolicy :: Policy -> [L.Decl]
processPolicy policy = preDecls ++ classDecls ++ classDecls' ++ domainDecls ++ connectionDecls
  where
    finalSt :: St
    finalSt = execState (mapM_ processTeRbac (teRbacs policy)) initSt
    activeClasses :: Set ClassId
    activeClasses =
      Set.unions $
        [ fromMaybe Set.empty (Map.lookup x (object_classes finalSt))
        | x <- Set.toList (actors finalSt) ]
    toPortId :: (ClassId, PermissionId) -> L.Name
    toPortId (cls, perm) = L.Name (idString cls ++ "_" ++ idString perm)
    toClassId :: ClassId -> L.Name
    toClassId = L.Name . capitalize . idString
    toIdentifier :: TypeOrAttributeId -> L.Name
    toIdentifier = L.Name . lowercase . idString
    mkPortDecl :: L.Name -> L.Decl
    mkPortDecl port = L.newPort port
    mkDomDecl :: L.Name -> L.Name -> L.Decl
    mkDomDecl dom cls = L.Domain dom cls []
    commonMap :: Map CommonId [PermissionId]
    commonMap = Map.fromList [ (i, toList ps) | CommonPerm i ps <- commonPerms policy ]
    activePort :: L.Name
    activePort = L.Name "active"
    subjectClass :: L.Name
    subjectClass = L.Name "SUBJECT"
    unknownClass :: L.Name
    unknownClass = L.Name "UNKNOWN"
    preDecls :: [L.Decl]
    preDecls =
        [ L.Class subjectClass [] [mkPortDecl activePort]
        , L.Class unknownClass [] [] ]
    classDecls :: [L.Decl]
    classDecls = map classDecl (toList (avPerms policy))
    classDecl :: AvPerm -> L.Decl
    classDecl (AvPermClass classId e) =
      L.Class (toClassId classId) [] (active ++ stmts)
      where
        isActive :: Bool
        isActive = Set.member classId activeClasses
        active :: [L.Decl]
        active = if isActive then [mkPortDecl activePort] else []
        perms :: [PermissionId]
        perms = case e of
                  Left ps -> toList ps
                  Right (commonId, ps) -> fromMaybe [] (Map.lookup commonId commonMap) ++ ps
        stmts :: [L.Decl]
        stmts = [ mkPortDecl (toPortId (classId, p)) | p <- perms ]
    classDecls' :: [L.Decl]
    classDecls' = concatMap classDecl' (Set.toList (all_types finalSt))
    classDecl' :: TypeOrAttributeId -> [L.Decl]
    classDecl' typeId =
      case fmap Set.toList $ Map.lookup typeId (object_classes finalSt) of
        Just [c] -> []
        _ -> [L.Class classId [] (active ++ stmts)]
          where
            classId :: L.Name
            classId = L.Name . ("TE_" ++) . idString $ typeId
            isActive :: Bool
            isActive = Set.member typeId (actors finalSt)
            active :: [L.Decl]
            active = if isActive then [mkPortDecl activePort] else []
            perms :: [(ClassId, PermissionId)]
            perms = Set.toList $ fromMaybe Set.empty $ Map.lookup typeId (object_perms finalSt)
            stmts :: [L.Decl]
            stmts = map (mkPortDecl . toPortId) perms
    domainDecls :: [L.Decl]
    domainDecls = map domainDecl (Set.toList (all_types finalSt))
    domainDecl :: TypeOrAttributeId -> L.Decl
    domainDecl typeId = mkDomDecl (toIdentifier typeId) classId
      where
        classId =
          case Map.lookup typeId (object_classes finalSt) of
            Nothing | Set.member typeId (actors finalSt) -> subjectClass
                    | otherwise                          -> unknownClass
            Just s -> case Set.toList s of
                        [c] -> toClassId c
                        _ -> L.Name . ("TE_" ++) . idString $ typeId
    connectionDecls :: [L.Decl]
    connectionDecls = map connectionDecl (Set.toList (allow_rules finalSt))
    connectionDecl :: AllowRule -> L.Decl
    connectionDecl allow =
        L.neutral
          (L.domPort (toIdentifier subject) activePort)
          (L.domPort (toIdentifier object) (toPortId (cls, perm)))
      where
        subject = allowSubject allow
        object  = allowObject allow
        cls     = allowClass allow
        perm    = allowPerm allow

processPolicyFile :: Prelude.FilePath -> IO String
processPolicyFile path = do
  s <- readFile path
  case parsePolicy path s of
    Left err -> return err
    Right pol -> return $ showLobster $ processPolicy pol

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

lowercase :: String -> String
lowercase "domain" = "domain_type" -- FIXME: ugly hack
lowercase "" = ""
lowercase (x:xs) = toLower x : xs

-- TODO: expand type attributes?

-- What does this rule mean?
-- allow domain self:dir { getattr search open read lock ioctl };

die :: String -> IO a
die err = do
  hPutStrLn stderr err
  exitFailure

parseArgs :: IO Prelude.FilePath
parseArgs = do
  args <- getArgs
  case args of
    []     -> die "usage: selinux-lobster FILENAME"
    (x:[]) -> return x

main :: IO ()
main = do
  filename <- parseArgs
  s <- readFile filename
  case parsePolicy filename s of
    Left err -> die err
    Right pol -> putStrLn $ showLobster $ processPolicy pol
