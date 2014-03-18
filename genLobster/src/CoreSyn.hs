{-# OPTIONS_GHC -Wall #-}
module CoreSyn
  ( Decl
  , Name
  , nameString
  , mkName
  , DomPort
  , Dir
  , Param
  , ConnectAnnotation
  , mkAnnotation
  , AnnotationElement
  , annotationInt
  , annotationString
  , annotationName

  , newClass
  , newDomain
  , newDomain'
  , newPort

  , domPort
  , extPort
  , left
  , right
  , neutral
  , bidi
  , connect
  , neutral'
  , connect'

  , showLobster
  ) where

import qualified Lobster.Core as L
import qualified Data.Text as Text
import qualified Text.PrettyPrint.Mainland as P

type Name = L.VarName L.Span
type Param = Name
type Dir = L.ConnType
type DomPort = L.PortName L.Span
type AnnotationElement = L.Exp L.Span
type ConnectAnnotation = L.AnnotationElement L.Span
type Decl = L.Stmt L.Span

mkName :: String -> Name
mkName s = L.VarName L.emptySpan (Text.pack s)

nameString :: Name -> String
nameString (L.VarName _ s) = Text.unpack s

newClass :: Name -> [Param] -> [Decl] -> Decl
newClass (L.VarName s c) ps body =
  L.StmtClassDecl L.emptySpan (L.TypeName s c) ps body

newPort :: Name -> Decl
newPort nm = L.StmtPortDecl L.emptySpan nm []

mkAnnotation :: Name -> [AnnotationElement] -> ConnectAnnotation
mkAnnotation (L.VarName s n) xs = (L.TypeName s n, xs)

annotationInt :: Int -> AnnotationElement
annotationInt = L.ExpInt . L.LitInteger L.emptySpan . toInteger

annotationString :: String -> AnnotationElement
annotationString = L.ExpString . L.LitString L.emptySpan . Text.pack

annotationName :: Name -> AnnotationElement
annotationName = L.ExpVar

annotateDecl :: [ConnectAnnotation] -> Decl -> Decl
annotateDecl xs = L.StmtAnnotation L.emptySpan (L.Annotation xs)

newDomain :: Name -> Name -> [Param] -> Decl
newDomain binder (L.VarName s c) args =
  L.StmtDomainDecl L.emptySpan binder (L.TypeName s c) (map L.ExpVar args)

newDomain' :: Name -> Name -> [Param] -> [ConnectAnnotation] -> Decl
newDomain' binder ctor args xs = annotateDecl xs (newDomain binder ctor args)

domPort :: Name -> Name -> DomPort
domPort a b = L.QPortName L.emptySpan a b

extPort :: Name -> DomPort
extPort b = L.UPortName b

left :: DomPort -> DomPort -> Decl
left = connect L.ConnRightToLeft

right :: DomPort -> DomPort -> Decl
right = connect L.ConnLeftToRight

neutral :: DomPort -> DomPort -> Decl
neutral = connect L.ConnNeutral

neutral' :: DomPort -> DomPort -> [ConnectAnnotation] -> Decl
neutral' = connect' L.ConnNeutral

bidi :: DomPort -> DomPort -> Decl
bidi = connect L.ConnBidirectional

connect :: Dir -> DomPort -> DomPort -> Decl
connect d a b = L.StmtConnection L.emptySpan a (L.ConnOp L.emptySpan d) b

connect' :: Dir -> DomPort -> DomPort -> [ConnectAnnotation] -> Decl
connect' d a b xs = annotateDecl xs (connect d a b)

showLobster :: [Decl] -> String
showLobster ds = P.pretty 0 (P.ppr (L.Policy L.emptySpan ds))
