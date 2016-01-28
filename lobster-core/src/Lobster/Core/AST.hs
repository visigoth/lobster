{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- AST.hs --- Front-end abstract syntax tree for Lobster.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

module Lobster.Core.AST
  ( Labeled(..)
  , Direction(..)
  , Position(..)
  , revPosition
  , ConnType(..)
  , LitInteger(..)
  , LitString(..)
  , getLitString
  , LitBool(..)
  , LitDirection(..)
  , LitPosition(..)
  , VarName(..)
  , getVarName
  , TypeName(..)
  , getTypeName
  , Qualified(..)
  , getModulePrefix
  , ConnOp(..)
  , PortAttr(..)
  , AnnotationElement
  , Annotation(..)
  , lookupAnnotation
  , lookupAnnotations
  , PortName(..)
  , Stmt(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , Exp(..)
  , _ExpInt
  , _ExpString
  , _ExpBool
  , _ExpDirection
  , _ExpPosition
  , _ExpVar
  , _ExpBinaryOp
  , _ExpUnaryOp
  , _ExpParen
  , Policy(..)
  , revConnType
  ) where

import Control.Arrow (first)
import Control.Lens
import Data.Foldable (Foldable)
import Data.Monoid ((<>), Monoid(..))
import Data.Text (Text)
import qualified Data.Text as T

-- | Class of AST objects labeled with arbitrary data.
class Labeled a where
  label :: a b -> b

----------------------------------------------------------------------
-- Atoms

-- | A direction value.
data Direction
  = DirInput
  | DirOutput
  | DirBidirectional
  deriving (Eq, Ord, Show)

-- | A position value.
data Position
  = PosSubject
  | PosObject
  | PosUnknown
  deriving (Eq, Ord, Show)

-- | Reverse a known position.
revPosition :: Position -> Position
revPosition PosSubject = PosObject
revPosition PosObject  = PosSubject
revPosition PosUnknown = PosUnknown

-- | The directionality of a connection.
data ConnType
  = ConnLeftToRight
  | ConnRightToLeft
  | ConnBidirectional
  | ConnNeutral
  | ConnNegative
  deriving (Eq, Ord, Show)

-- | Reverse the directionality of a connection.
revConnType :: ConnType -> ConnType
revConnType ConnLeftToRight = ConnRightToLeft
revConnType ConnRightToLeft = ConnLeftToRight
revConnType c               = c

-- | A boolean literal.
data LitBool a = LitBool a Bool
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Labeled LitBool where
  label (LitBool l _) = l

-- | An integer literal.
data LitInteger a = LitInteger a Integer
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Labeled LitInteger where
  label (LitInteger l _) = l

-- | A string literal.
data LitString a = LitString a Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Extract the string from a string literal.
getLitString :: LitString a -> Text
getLitString (LitString _ x) = x

instance Labeled LitString where
  label (LitString l _) = l

-- | A direction literal.
data LitDirection a = LitDirection a Direction
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Labeled LitDirection where
  label (LitDirection l _) = l

-- | A position literal.
data LitPosition a = LitPosition a Position
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Labeled LitPosition where
  label (LitPosition l _) = l

-- | A lower case variable name identifier.
data VarName a = VarName a Text
  deriving (Show, Functor, Foldable, Traversable)

-- | Extract the text from a variable name.
getVarName :: VarName a -> Text
getVarName (VarName _ t) = t

instance Eq (VarName a) where
  VarName _ n1 == VarName _ n2 = n1 == n2

instance Ord (VarName a) where
  compare (VarName _ n1) (VarName _ n2) = compare n1 n2

instance Labeled VarName where
  label (VarName l _) = l

-- | An upper case type name identifier.
data TypeName a = TypeName a Text
  deriving (Show, Functor, Foldable, Traversable)

-- | Extract the text from a variable name.
getTypeName :: TypeName a -> Text
getTypeName (TypeName _ t) = t

instance Eq (TypeName a) where
  TypeName _ t1 == TypeName _ t2 = t1 == t2

instance Ord (TypeName a) where
  compare (TypeName _ n1) (TypeName _ n2) = compare n1 n2

instance Labeled TypeName where
  label (TypeName l _) = l

-- | Qualified name: a variable or type optionally prefixed by a module path
data Qualified b a = Qualified a (Maybe [VarName a]) (b a)
  deriving (Show, Functor, Foldable, Traversable)

getModulePrefix :: Qualified b a -> Text
getModulePrefix (Qualified _ Nothing     _) = ""
getModulePrefix (Qualified _ (Just mods) _) =
  T.intercalate "::" (fmap getVarName mods) <> "::"

instance Eq (b a) => Eq (Qualified b a) where
  Qualified _ mods1 ident1 == Qualified _ mods2 ident2 =
    mods1 == mods2 && ident1 == ident2

instance (Eq (b a), Ord (b a)) => Ord (Qualified b a) where
  compare (Qualified _ mods1 ident1) (Qualified _ mods2 ident2) =
    case compare mods1 mods2 of
      EQ -> compare ident1 ident2
      o  -> o

instance Labeled (Qualified b) where
  label (Qualified l _ _) = l

-- | A connectivity operator.
data ConnOp a = ConnOp a ConnType
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Labeled ConnOp where
  label (ConnOp l _) = l

----------------------------------------------------------------------
-- Statements

-- | An attribute of a port definition.
data PortAttr a = PortAttr (VarName a) (Maybe (Exp a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | A single annotation on a statement.
type AnnotationElement a = (TypeName a, [Exp a])

-- | A metadata annotation for a statement.
data Annotation a = Annotation [AnnotationElement a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Monoid (Annotation a) where
  mempty = Annotation []
  mappend (Annotation xs) (Annotation ys) = Annotation (xs <> ys)

-- | Look up an annotation by name.  If there are multiple
-- annotations with the same name, only the first is returned.
lookupAnnotation :: Text -> Annotation a -> Maybe [Exp a]
lookupAnnotation t (Annotation anns) = lookup t (map (first getTypeName) anns)

-- | Look up a list of annotations by name.
lookupAnnotations :: Text -> Annotation a -> [[Exp a]]
lookupAnnotations t (Annotation anns) = map snd matches
  where
    matches = filter go anns
    go (TypeName _ name, _) = t == name

-- | A port reference (qualified or unqualified).
data PortName a
  = UPortName (VarName a)
  | QPortName a (Qualified VarName a) (VarName a)
  deriving (Show, Functor, Foldable, Traversable)

-- | Convert a port ID to a tuple, discarding the label.
portNameToTuple :: PortName l -> (Maybe ([VarName l], VarName l), VarName l)
portNameToTuple (UPortName     r) = (Nothing, r)
portNameToTuple (QPortName _ (Qualified _ Nothing     _) r) = (Nothing, r)
portNameToTuple (QPortName _ (Qualified _ (Just mods) l) r) = (Just (mods, l), r)

instance Eq (PortName l) where
  (==) a b = portNameToTuple a == portNameToTuple b

instance Ord (PortName l) where
  compare a b = portNameToTuple a `compare` portNameToTuple b

instance Labeled PortName where
  label (UPortName x) = label x
  label (QPortName l _ _) = l

-- | A top-level statement in a Lobster module.
data Stmt a
  = StmtModuleDecl     a (VarName a) [Stmt a]
  | StmtClassDecl      a Bool (TypeName a) [VarName a] [Stmt a]
  | StmtPortDecl       a (VarName a) [PortAttr a]
  | StmtDomainDecl     a (VarName a) (TypeName a) [Exp a]
  | StmtAnonDomainDecl a Bool (VarName a) [Stmt a]
  | StmtAssign         a (VarName a) (Exp a)
  | StmtConnection     a (PortName a) (ConnOp a) (PortName a)
  | StmtAnnotation     a (Annotation a) (Stmt a)
  | StmtComment        a Text     -- never generated by parser
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Labeled Stmt where
  label (StmtModuleDecl     l _ _)     = l
  label (StmtClassDecl      l _ _ _ _) = l
  label (StmtPortDecl       l _ _)     = l
  label (StmtDomainDecl     l _ _ _)   = l
  label (StmtAnonDomainDecl l _ _ _)   = l
  label (StmtAssign         l _ _)     = l
  label (StmtConnection     l _ _ _)   = l
  label (StmtAnnotation     l _ _)     = l
  label (StmtComment        l _)       = l

-- | A policy file, contains a list of statements.
data Policy a = Policy a [Stmt a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

----------------------------------------------------------------------
-- Assertions

-- TODO: Come up with good AST for assertions, maybe look at CoreSyn?

----------------------------------------------------------------------
-- Expressions

-- | A unary operator (currently only logical NOT).
data UnaryOp
  = UnaryOpNot
  deriving (Eq, Ord, Show)

-- | A binary operator (currently only for booleans).
data BinaryOp
  = BinaryOpAnd
  | BinaryOpOr
  | BinaryOpEqual
  | BinaryOpNotEqual
  deriving (Eq, Ord, Show)

-- | An expression.
data Exp a
  = ExpInt        (LitInteger a)
  | ExpString     (LitString a)
  | ExpBool       (LitBool a)
  | ExpDirection  (LitDirection a)
  | ExpPosition   (LitPosition a)
  | ExpVar        (VarName a)
  | ExpBinaryOp   a (Exp a) BinaryOp (Exp a)
  | ExpUnaryOp    a UnaryOp (Exp a)
  | ExpParen      a (Exp a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makePrisms ''Exp

instance Labeled Exp where
  label (ExpInt x) = label x
  label (ExpString x) = label x
  label (ExpBool x) = label x
  label (ExpDirection x) = label x
  label (ExpPosition x) = label x
  label (ExpVar x) = label x
  label (ExpBinaryOp l _ _ _) = l
  label (ExpUnaryOp l _ _) = l
  label (ExpParen l _) = l
