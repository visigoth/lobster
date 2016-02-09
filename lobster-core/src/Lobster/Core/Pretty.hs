{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--
-- Pretty.hs --- Pretty-printer for the Lobster AST.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

-- | This module only exports instances of 'Pretty' for Lobster
-- AST types.
module Lobster.Core.Pretty () where

import Data.Monoid (mconcat)
import Text.PrettyPrint.Mainland

import Lobster.Core.AST

----------------------------------------------------------------------
-- Utilities

-- | Comma separate a list of pretty printable things.
pprCommaSep :: Pretty a => [a] -> Doc
pprCommaSep = commasep . map ppr

----------------------------------------------------------------------
-- Lobster Data Types

instance Pretty Direction where
  ppr DirInput          = text "input"
  ppr DirOutput         = text "output"
  ppr DirBidirectional  = text "bidirectional"

instance Pretty Position where
  ppr PosSubject        = text "subject"
  ppr PosObject         = text "object"
  ppr PosUnknown        = error "printing unknown position"

instance Pretty ConnType where
  ppr ConnLeftToRight   = text "-->"
  ppr ConnRightToLeft   = text "<--"
  ppr ConnBidirectional = text "<-->"
  ppr ConnNeutral       = text "--"
  ppr ConnNegative      = text "-/-"

instance Pretty BinaryOp where
  ppr BinaryOpAnd      = text "&&"
  ppr BinaryOpOr       = text "||"
  ppr BinaryOpEqual    = text "=="
  ppr BinaryOpNotEqual = text "!="

instance Pretty UnaryOp where
  ppr UnaryOpNot = text "!"

----------------------------------------------------------------------
-- AST Atoms

instance Pretty (LitInteger a) where
  ppr (LitInteger _ x) = ppr x

instance Pretty (LitString a) where
  ppr (LitString _ x) = ppr x

instance Pretty (LitBool a) where
  ppr (LitBool _ True)  = text "true"
  ppr (LitBool _ False) = text "false"

instance Pretty (LitDirection a) where
  ppr (LitDirection _ x) = ppr x

instance Pretty (LitPosition a) where
  ppr (LitPosition _ x) = ppr x

instance Pretty (VarName a) where
  ppr (VarName _ x) = fromText x

instance Pretty (VarPattern a) where
  ppr (AnyPattern _)       = text "*"
  ppr (IdentPattern ident) = ppr ident

instance Pretty (TypeName a) where
  ppr (TypeName _ x) = fromText x

instance Pretty (a l) => Pretty (Qualified a l) where
  ppr qual = fromText (getQualifierPrefix qual) <> ppr (getUnqualified qual)

instance Pretty (ConnOp a) where
  ppr (ConnOp _ x) = ppr x

instance Pretty (FlowPred a) where
  ppr (NeverPathFP _)  = text "never"
  ppr (ExistsPathFP _) = text "exists"
  ppr (PathFP xs)      = mconcat (fmap ppr xs)

----------------------------------------------------------------------
-- Annotations

pprAnnotationElement :: AnnotationElement a -> Doc
pprAnnotationElement (tyName, args) =
  ppr tyName <> parens (pprCommaSep args)

instance Pretty (Annotation a) where
  ppr (Annotation anns) =
    brackets (commasep (map pprAnnotationElement anns))

----------------------------------------------------------------------
-- Statements

instance Pretty (PortAttr a) where
  ppr (PortAttr name Nothing)  = ppr name <> text "=" <> text "*"
  ppr (PortAttr name (Just e)) = ppr name <> text "=" <> ppr e

ppExplicit :: Bool -> Doc
ppExplicit True  = text "explicit" <> space
ppExplicit False = empty

instance Pretty (Stmt a) where
  -- Printing empty classes different from non-empty here
  -- to get idiomatic output.
  ppr (StmtClassDecl _ isExp name args []) =
    ppExplicit isExp <>  text "class"
                     <+> ppr name
                     <> parens (pprCommaSep args)
                     <+> braces empty
  ppr (StmtClassDecl _ isExp name args body) =
    ppExplicit isExp <>  text "class"
                     <+> ppr name
                     <>  parens (pprCommaSep args)
                     </> lbrace
                     </> indent 2 (pprStmts body)
                     </> rbrace
  ppr (StmtModuleDecl _ varName body) =
    text "mod" <> ppr varName <> parens (pprCommaSep body)
  ppr (StmtPortDecl _ name []) =
    text "port" <+> ppr name <> semi
  ppr (StmtPortDecl _ name attrs) =
    text "port" <+> ppr name <+> colon
                <+> lbrace <+> pprCommaSep attrs <+> rbrace
                <>  semi
  ppr (StmtDomainDecl _ varName tyName args) =
    text "domain" <+> ppr varName <+> equals <+> ppr tyName
                  <> parens (pprCommaSep args) <> semi
  ppr (StmtAnonDomainDecl _ isExp varName body) =
    ppExplicit isExp <>  text "domain"
                     <+> ppr varName <+> equals <+> lbrace
                     </> indent 2 (pprStmts body)
                     </> rbrace
                     <>  semi
  ppr (StmtAssign _ varName expr) =
    ppr varName <+> equals <+> ppr expr <> semi
  ppr (StmtConnection _ e1 conn e2) =
    ppr e1 <+> ppr conn <+> ppr e2 <> semi
  ppr (StmtAnnotation _ a stmt) =
    ppr a </> ppr stmt
  ppr (StmtAssertion _ ldom rdom fp) =
    let br x = text "[" <> ppr x <> text "]"
    in text "assert" <> br ldom <> text "->" <> br rdom <> text "::" <> ppr fp <> semi
  ppr (StmtComment _ a) =
    -- TODO: linewrap long comments
    text "//" <+> fromText a

-- | Return true if two statements should be grouped
-- together without an extra newline.
--
-- In general, we separate class definitions from everything
-- unless both are empty-arg empty-body classes.  Other
-- statements are grouped if they are the same kind of statement.
--
-- Statements with annotations are preceded by a blank line.
stmtGroup :: Stmt a -> Stmt a -> Bool
stmtGroup (StmtClassDecl _ _ _ [] []) (StmtClassDecl _ _ _ [] []) = True
stmtGroup StmtClassDecl{}  StmtClassDecl{}  = False
stmtGroup StmtPortDecl{}   StmtPortDecl{}   = True
stmtGroup StmtDomainDecl{} StmtDomainDecl{} = True
stmtGroup StmtDomainDecl{} StmtConnection{} = True    -- looks nice
stmtGroup StmtAssign{}     StmtAssign{}     = True
stmtGroup StmtConnection{} StmtConnection{} = True
stmtGroup _                StmtAnnotation{} = False
stmtGroup StmtComment{}    _                = True
stmtGroup _                _                = False

-- | Pretty print a list of policy statements.
pprStmts :: [Stmt a] -> Doc
pprStmts []     = empty
pprStmts (x:[]) = ppr x
pprStmts (s1:rest@(s2:_))
  | stmtGroup s1 s2 = ppr s1 <> line <> pprStmts rest
  | otherwise       = ppr s1 <> line <> line <> pprStmts rest

instance Pretty (Policy a) where
  ppr (Policy _ stmts) = pprStmts stmts

----------------------------------------------------------------------
-- Expressions

data Fixity = Fixity Assoc Int
  deriving (Eq, Ord)

data Assoc = LeftAssoc | RightAssoc | NonAssoc
  deriving (Eq, Ord)

fixity :: BinaryOp -> Fixity
fixity BinaryOpOr       = Fixity RightAssoc 2
fixity BinaryOpAnd      = Fixity RightAssoc 3
fixity BinaryOpEqual    = Fixity NonAssoc 4
fixity BinaryOpNotEqual = Fixity NonAssoc 4

pprBinOp :: Int -> Exp a -> BinaryOp -> Exp a -> Doc
pprBinOp prec e1 op e2 =
  parensIf (prec > opPrec) $
    pprPrec leftPrec e1 <+> ppr op <+> pprPrec rightPrec e2
  where
    leftPrec | opAssoc == RightAssoc = opPrec + 1
             | otherwise             = opPrec
    rightPrec | opAssoc == LeftAssoc = opPrec + 1
              | otherwise            = opPrec
    Fixity opAssoc opPrec = fixity op

pprUnaryOp :: Int -> UnaryOp -> Exp a -> Doc
pprUnaryOp prec op e =
  parensIf (prec > 14) $
    ppr op <> pprPrec 14 e

instance Pretty (Exp a) where
  pprPrec _ (ExpInt x)               = ppr x
  pprPrec _ (ExpString s)            = ppr s
  pprPrec _ (ExpBool x)              = ppr x
  pprPrec _ (ExpDirection x)         = ppr x
  pprPrec _ (ExpPosition x)          = ppr x
  pprPrec _ (ExpVar x)               = ppr x
  pprPrec n (ExpBinaryOp _ e1 op e2) = pprBinOp n e1 op e2
  pprPrec n (ExpUnaryOp _ op e)      = pprUnaryOp n op e
  pprPrec _ (ExpParen _ x)           = parens (ppr x)

