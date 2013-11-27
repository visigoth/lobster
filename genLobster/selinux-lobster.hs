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

import qualified Lobster.Abs as L
import qualified Lobster.Print as P

import SCD.SELinux.Syntax
import SCD.SELinux.Parser (parsePolicy)

data St = St
  { actors         :: !(Set TypeOrAttributeId)
  , all_types      :: !(Set TypeOrAttributeId)
  , object_classes :: !(Map TypeOrAttributeId (Set ClassId))
  , class_perms    :: !(Map ClassId (Set PermissionId))
  , allow_rules    :: [(TypeOrAttributeId, TypeOrAttributeId, PermissionId)]
  }

initSt :: St
initSt = St
  { actors         = Set.empty
  , all_types      = Set.empty
  , object_classes = Map.empty
  , class_perms    = Map.empty
  , allow_rules    = []
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
addAllow subject object classId perms = modify f
  where
    f st = St
      { actors = Set.insert subject (actors st)
      , all_types = Set.insert subject (Set.insert object (all_types st))
      , object_classes = insertMapSet object classId (object_classes st)
      , class_perms = Map.insertWith (flip Set.union) classId perms (class_perms st)
      , allow_rules = [ (subject, object, perm) | perm <- Set.toList perms ] ++ allow_rules st
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

processPolicy :: Policy -> L.Policy
processPolicy policy = L.Policy (preDecls ++ classDecls ++ classDecls' ++ domainDecls ++ connectionDecls)
  where
    finalSt :: St
    finalSt = execState (mapM_ processTeRbac (teRbacs policy)) initSt
    activeClasses :: Set ClassId
    activeClasses =
      Set.unions $
        [ fromMaybe Set.empty (Map.lookup x (object_classes finalSt))
        | x <- Set.toList (actors finalSt) ]
    toPortId :: PermissionId -> L.PortId
    toPortId = L.PortId . L.LIdent . idString
    toClassId :: ClassId -> L.ClassId
    toClassId = L.ClassId . L.UIdent . capitalize . idString
    toIdentifier :: TypeOrAttributeId -> L.Identifier
    toIdentifier = L.Identifier . L.LIdent . lowercase . idString
    mkPortDecl :: L.PortId -> L.Statement
    mkPortDecl port = L.PortDeclaration port L.EmptyPDT L.EmptyPDC
    mkDomDecl :: L.Identifier -> L.ClassId -> L.Statement
    mkDomDecl dom cls = L.DomainDeclaration dom (L.ClassInstantiation cls [])
    commonMap :: Map CommonId [PermissionId]
    commonMap = Map.fromList [ (i, toList ps) | CommonPerm i ps <- commonPerms policy ]
    activePort :: L.PortId
    activePort = L.PortId (L.LIdent "active")
    subjectClass :: L.ClassId
    subjectClass = (L.ClassId (L.UIdent "SUBJECT"))
    unknownClass :: L.ClassId
    unknownClass = (L.ClassId (L.UIdent "UNKNOWN"))
    preDecls :: [L.Statement]
    preDecls =
        [ L.ClassDeclaration subjectClass [] [mkPortDecl activePort]
        , L.ClassDeclaration unknownClass [] [] ]
    classDecls :: [L.Statement]
    classDecls = map classDecl (toList (avPerms policy))
    classDecl :: AvPerm -> L.Statement
    classDecl (AvPermClass classId e) =
      L.ClassDeclaration (toClassId classId) [] (active ++ stmts)
      where
        isActive :: Bool
        isActive = Set.member classId activeClasses
        active :: [L.Statement]
        active = if isActive then [mkPortDecl activePort] else []
        perms :: [PermissionId]
        perms = case e of
                  Left ps -> toList ps
                  Right (commonId, ps) -> fromMaybe [] (Map.lookup commonId commonMap) ++ ps
        stmts :: [L.Statement]
        stmts = map (mkPortDecl . toPortId) perms
    classDecls' :: [L.Statement]
    classDecls' = concatMap classDecl' (Set.toList (all_types finalSt))
    classDecl' :: TypeOrAttributeId -> [L.Statement]
    classDecl' typeId =
      case fmap Set.toList $ Map.lookup typeId (object_classes finalSt) of
        Just [c] -> []
        _ -> [L.ClassDeclaration classId [] (active ++ stmts)]
          where
            classId :: L.ClassId
            classId = L.ClassId . L.UIdent . ("TE_" ++) . idString $ typeId
            isActive :: Bool
            isActive = Set.member typeId (actors finalSt)
            active :: [L.Statement]
            active = if isActive then [mkPortDecl activePort] else []
            perms :: [PermissionId]
            perms = Set.toList $ Set.fromList [ p | (_, t, p) <- allow_rules finalSt, t == typeId ]
            stmts :: [L.Statement]
            stmts = map (mkPortDecl . toPortId) perms
    domainDecls :: [L.Statement]
    domainDecls = map domainDecl (Set.toList (all_types finalSt))
    domainDecl :: TypeOrAttributeId -> L.Statement
    domainDecl typeId = mkDomDecl (toIdentifier typeId) classId
      where
        classId =
          case Map.lookup typeId (object_classes finalSt) of
            Nothing | Set.member typeId (actors finalSt) -> subjectClass
                    | otherwise                          -> unknownClass
            Just s -> case Set.toList s of
                        [c] -> toClassId c
                        _ -> L.ClassId . L.UIdent . ("TE_" ++) . idString $ typeId
    connectionDecls :: [L.Statement]
    connectionDecls = map connectionDecl (reverse (allow_rules finalSt))
    connectionDecl :: (TypeOrAttributeId, TypeOrAttributeId, PermissionId) -> L.Statement
    connectionDecl (subject, object, perm) =
        L.PortConnection
          [toExpression subject activePort]
          L.NeutralConnection
          [toExpression object (toPortId perm)]
    toExpression :: TypeOrAttributeId -> L.PortId -> L.Expression
    toExpression typeId (L.PortId lident) = L.QualNameExpression (L.Qual (L.UnQual domName) portName)
      where
        domName = L.Ident (toIdentifier typeId)
        portName = L.Ident (L.Identifier lident)

processPolicyFile :: Prelude.FilePath -> IO String
processPolicyFile path = do
  s <- readFile path
  case parsePolicy path s of
    Left err -> return err
    Right pol -> return $ P.printTree $ processPolicy pol

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
    Right pol -> putStrLn $ P.printTree $ processPolicy pol
