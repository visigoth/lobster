{-# OPTIONS -Wall -Werror #-}
{- |
Substition and macro expansion for Shrimp M4 subset
-}
module SCD.M4.Subst
  ( Subst(..)
  , Substitution
  , Macros(..)
  , Expand(..)
  , expandPolicyModule
  ) where

import Data.Char (isDigit)
import Data.Foldable (Foldable, toList)
import Data.Map (Map)
import Data.NonEmptyList (NonEmptyList, FromList(..))
import qualified Data.Map as Map

import SCD.M4.Syntax hiding (avPerms)
import qualified SCD.M4.Syntax as M4

import qualified SCD.SELinux.Syntax as S

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
