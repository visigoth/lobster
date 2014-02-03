module Lobster.AST where

newtype LIdent = LIdent String deriving (Eq,Ord,Show)
newtype UIdent = UIdent String deriving (Eq,Ord,Show)
data Policy a =
   Policy [Statement a]
  deriving (Eq,Ord,Show)

data Statement a =
   ClassDeclaration a ClassId [Identifier] [Statement a]
 | PortDeclaration a PortId PortDeclarationType PortDeclarationConnection
 | DomainDeclaration a Identifier ClassInstantiation
 | Assignment a Identifier Expression
 | PortConnection a [Expression] Connection [Expression]
 | Assert a ConnRE ConnRE FlowPred
  deriving (Eq,Ord,Show)

data ClassInstantiation =
   ClassInstantiation ClassId [Expression]
  deriving (Eq,Ord,Show)

data ConnRE =
   ConnRE DomainSpec PortRE
  deriving (Eq,Ord,Show)

data DomainSpec =
   ThisDom
 | IdentDom Identifier
  deriving (Eq,Ord,Show)

data PortRE =
   AnyPRE
 | IdPRE Identifier
  deriving (Eq,Ord,Show)

data FlowPred =
   NeverPathFP
 | ExistsPathFP
 | PathFP FlowRE
  deriving (Eq,Ord,Show)

data FlowRE =
   ConsF FlowRE ConnRE FlowRE
 | AnyFRE
  deriving (Eq,Ord,Show)

data PortDeclarationType =
   EmptyPDT
 | PortTypePDT [PortTypeConstraint]
  deriving (Eq,Ord,Show)

data PortDeclarationConnection =
   EmptyPDC
 | Connection Connection [Expression]
  deriving (Eq,Ord,Show)

data Expression =
   IntExpression Integer
 | StringExpression String
 | DirectionExpression Direction
 | PositionExpression Position
 | QualNameExpression QualName
 | ParenExpression Expression
  deriving (Eq,Ord,Show)

data Direction =
   InputDirection
 | OutputDirection
 | BidirectionalDirection
  deriving (Eq,Ord,Show)

data Position =
   SubjectPosition
 | ObjectPosition
  deriving (Eq,Ord,Show)

data QualName =
   UnQual Name
 | Qual QualName Name
  deriving (Eq,Ord,Show)

data Name =
   TypeIdent ClassId
 | Ident Identifier
  deriving (Eq,Ord,Show)

data PortTypeConstraint =
   PortTypeConstraint FlowId NoneExpression
  deriving (Eq,Ord,Show)

data NoneExpression =
   NoneE
 | SomeE Expression
  deriving (Eq,Ord,Show)

data Connection =
   BidirectionalConnection
 | LeftToRightConnection
 | RightToLeftConnection
 | NeutralConnection
  deriving (Eq,Ord,Show)

data Identifier =
   Identifier LIdent
  deriving (Eq,Ord,Show)

data PortId =
   PortId LIdent
  deriving (Eq,Ord,Show)

data FlowId =
   FlowId LIdent
  deriving (Eq,Ord,Show)

data ClassId =
   ClassId UIdent
  deriving (Eq,Ord,Show)
