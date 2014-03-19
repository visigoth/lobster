{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
--
-- AST.hs --- Front-end abstract syntax tree for Lobster.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Lobster.Core.AST
  ( Labeled(..)
  , Direction(..)
  , Position(..)
  , ConnType(..)
  , LitInteger(..)
  , LitString(..)
  , LitDirection(..)
  , LitPosition(..)
  , VarName(..)
  , getVarName
  , TypeName(..)
  , getTypeName
  , ConnOp(..)
  , PortAttr(..)
  , AnnotationElement
  , Annotation(..)
  , lookupAnnotation
  , PortName(..)
  , Stmt(..)
  , Exp(..)
  , Policy(..)
  , revConnType
  ) where

import Control.Arrow (first)
import Data.Monoid ((<>), Monoid(..))
import Data.Text (Text)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

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
  deriving (Eq, Ord, Show)

-- | The directionality of a connection.
data ConnType
  = ConnLeftToRight
  | ConnRightToLeft
  | ConnBidirectional
  | ConnNeutral
  deriving (Eq, Ord, Show)

-- | Reverse the directionality of a connection.
revConnType :: ConnType -> ConnType
revConnType ConnLeftToRight = ConnRightToLeft
revConnType ConnRightToLeft = ConnLeftToRight
revConnType c               = c

-- | An integer literal.
data LitInteger a = LitInteger a Integer
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Labeled LitInteger where
  label (LitInteger l _) = l

-- | A string literal.
data LitString a = LitString a Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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

-- | Look up an annotation by name.
lookupAnnotation :: Text -> Annotation a -> Maybe [Exp a]
lookupAnnotation t (Annotation anns) = lookup t (map (first getTypeName) anns)

-- | A port reference (qualified or unqualified).
data PortName a
  = UPortName (VarName a)
  | QPortName a (VarName a) (VarName a)
  deriving (Show, Functor, Foldable, Traversable)

-- | Convert a port ID to a tuple, discarding the label.
portNameToTuple :: PortName l -> (Maybe (VarName l), VarName l)
portNameToTuple (UPortName     r) = (Nothing, r)
portNameToTuple (QPortName _ l r) = (Just l, r)

instance Eq (PortName l) where
  (==) a b = portNameToTuple a == portNameToTuple b

instance Ord (PortName l) where
  compare a b = portNameToTuple a `compare` portNameToTuple b

instance Labeled PortName where
  label (UPortName x) = label x
  label (QPortName l _ _) = l

-- | A top-level statement in a Lobster module.
data Stmt a
  = StmtClassDecl   a (TypeName a) [VarName a] [Stmt a]
  | StmtPortDecl    a (VarName a) [PortAttr a]
  | StmtDomainDecl  a (VarName a) (TypeName a) [Exp a]
  | StmtAssign      a (VarName a) (Exp a)
  | StmtConnection  a (PortName a) (ConnOp a) (PortName a)
  | StmtAnnotation  a (Annotation a) (Stmt a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Labeled Stmt where
  label (StmtClassDecl  l _ _ _) = l
  label (StmtPortDecl   l _ _)   = l
  label (StmtDomainDecl l _ _ _) = l
  label (StmtAssign     l _ _)   = l
  label (StmtConnection l _ _ _) = l
  label (StmtAnnotation l _ _)   = l

-- | A policy file, contains a list of statements.
data Policy a = Policy a [Stmt a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

----------------------------------------------------------------------
-- Assertions

-- TODO: Come up with good AST for assertions, maybe look at CoreSyn?

----------------------------------------------------------------------
-- Expressions

-- | An expression.
data Exp a
  = ExpInt        (LitInteger a)
  | ExpString     (LitString a)
  | ExpDirection  (LitDirection a)
  | ExpPosition   (LitPosition a)
  | ExpVar        (VarName a)
  | ExpParen      a (Exp a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Labeled Exp where
  label (ExpInt x) = label x
  label (ExpString x) = label x
  label (ExpDirection x) = label x
  label (ExpPosition x) = label x
  label (ExpVar x) = label x
  label (ExpParen l _) = l

