{-# OPTIONS_GHC -Wall #-}
{- |
Module      :  CSD.Lobster.Gen.CoreSyn
Description :  Simply representing Lobster.
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  The SCD team
Stability   :  provisional
Portability :  portable

Stripped down representation of Lobster specs, for easier
generation and manipulation by programs. That's the intent,
anyway! :-)

-}
module SCD.Lobster.Gen.CoreSyn
       ( Module
       , Decl(..)
       , Name(..)
       , nameString
       , DomPort(..)
       , Dir(..)
       , Param
       , PortConstraint(..)
       , ConnectAnnotation(..)
       , AnnotationElement(..)

       , newClass
       , newType
       , newDomain
       , newPort
       , newComment

       , withConstraint
       , withConnection
       , dirPort

       , domPort
       , extPort
       , left
       , right
       , neutral
       , bidi
       , connect
       , connect'
       ) where

import Data.Monoid
import Data.String

-- | @Module@ associates a name with Lobster declaration
-- list; the name could then be used to derive a filename/URL
-- for output..
type Module = (Name,[Decl])

data Decl
   -- I can't help it, but the use of 'statements' in Lobster
   -- seems odd. It's all about wiring things up, so
   -- an operational model or the notion of 'executing' the
   -- spec seems
 = Class   Name [Param] [Decl]
 | Port    Name [PortConstraint] (Dir,[DomPort])
 | Domain  Name Name [Param]
 | Type    Name [Name]
 | Connect DomPort DomPort Dir [ConnectAnnotation]
 | Comment String

newtype Name = Name String
  deriving (Eq, Ord)

instance Show Name where
  show = nameString

instance IsString Name where
  fromString = Name

instance Monoid Name where
  mempty                        = fromString ""
  (Name n1) `mappend` (Name n2) = Name (n1 `mappend` n2)

nameString :: Name -> String
nameString (Name s) = s

type PortDecl = Decl

newClass :: Name -> [Param] -> [Decl] -> Decl
newClass n ps body = Class n ps body

newType :: Name -> [Name] -> Decl
newType x as = Type x as

newComment :: String -> Decl
newComment = Comment

newPort :: Name -> PortDecl
newPort nm = Port nm [] (N,[])

withConstraint :: PortConstraint -> PortDecl -> PortDecl
withConstraint pc (Port nm cs ds) = Port nm (pc:cs) ds

withConnection :: DomPort -> Dir -> PortDecl -> PortDecl
withConnection dp d (Port nm cs (_,ds)) = Port nm cs (d,dp:ds)

dirPort :: Name -> Dir -> PortDecl
dirPort nm d = withConstraint (PortDir d) (newPort nm)

data DomPort
 = DomPort
     { portDomain :: Maybe Name
     , portLabel  :: Name
     }
  deriving (Eq, Ord)

instance Show DomPort where
  show DomPort { portDomain = Nothing, portLabel = l } = show l
  show DomPort { portDomain = Just d, portLabel = l } = show d ++ "." ++ show l


data Dir = L | R | N | B

type Param = Name -- for now..

data PortConstraint
 = PortDir Dir
 | PortPos Bool  -- False => subject; True => ....yep,you guessed it..
 | PortType Name

data ConnectAnnotation = ConnectAnnotation Name [AnnotationElement]

data AnnotationElement = AnnotationInt Int | AnnotationString String

newDomain :: Name -> Name -> [Param] -> Decl
newDomain binder ctor args = Domain binder ctor args

domPort :: Name -> Name -> DomPort
domPort a b = DomPort { portDomain = Just a, portLabel = b }

extPort :: Name -> DomPort
extPort b = DomPort { portDomain = Nothing, portLabel = b }

left :: DomPort -> DomPort -> Decl
left = connect L

right :: DomPort -> DomPort -> Decl
right = connect R

neutral :: DomPort -> DomPort -> Decl
neutral = connect N

bidi :: DomPort -> DomPort -> Decl
bidi = connect B

connect :: Dir -> DomPort -> DomPort -> Decl
connect d a b = Connect a b d []

connect' :: Dir -> DomPort -> DomPort -> [ConnectAnnotation] -> Decl
connect' d a b xs = Connect a b d xs
