{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Lobster.PrettyPrint where

import Lobster.AST
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print LIdent where
  prt _ (LIdent i) = doc (showString ( i))


instance Print UIdent where
  prt _ (UIdent i) = doc (showString ( i))



instance Print (Policy a) where
  prt i e = case e of
   Policy statements -> prPrec i 0 (concatD [prt 0 statements])


instance Print (Statement a) where
  prt i e = case e of
   ClassDeclaration _ classid identifiers statements -> prPrec i 0 (concatD [doc (showString "class") , prt 0 classid , doc (showString "(") , prt 0 identifiers , doc (showString ")") , doc (showString "{") , prt 0 statements , doc (showString "}")])
   PortDeclaration _ portid portdeclarationtype portdeclarationconnection -> prPrec i 0 (concatD [doc (showString "port") , prt 0 portid , prt 0 portdeclarationtype , prt 0 portdeclarationconnection , doc (showString ";")])
   DomainDeclaration _ identifier classinstantiation -> prPrec i 0 (concatD [doc (showString "domain") , prt 0 identifier , doc (showString "=") , prt 0 classinstantiation , doc (showString ";")])
   Assignment _ identifier expression -> prPrec i 0 (concatD [prt 0 identifier , doc (showString "=") , prt 0 expression , doc (showString ";")])
   PortConnection _ expressions0 connection expressions -> prPrec i 0 (concatD [prt 0 expressions0 , prt 0 connection , prt 0 expressions , doc (showString ";")])
   Assert _ connre0 connre flowpred -> prPrec i 0 (concatD [doc (showString "assert") , prt 0 connre0 , doc (showString "->") , prt 0 connre , doc (showString "::") , prt 0 flowpred , doc (showString ";")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print ClassInstantiation where
  prt i e = case e of
   ClassInstantiation classid expressions -> prPrec i 0 (concatD [prt 0 classid , doc (showString "(") , prt 0 expressions , doc (showString ")")])


instance Print ConnRE where
  prt i e = case e of
   ConnRE domainspec portre -> prPrec i 0 (concatD [doc (showString "[") , prt 0 domainspec , prt 0 portre , doc (showString "]")])


instance Print DomainSpec where
  prt i e = case e of
   ThisDom  -> prPrec i 0 (concatD [doc (showString "this")])
   IdentDom identifier -> prPrec i 0 (concatD [prt 0 identifier])


instance Print PortRE where
  prt i e = case e of
   AnyPRE  -> prPrec i 0 (concatD [doc (showString ".*")])
   IdPRE identifier -> prPrec i 0 (concatD [doc (showString ".") , prt 0 identifier])


instance Print FlowPred where
  prt i e = case e of
   NeverPathFP  -> prPrec i 0 (concatD [doc (showString "never")])
   ExistsPathFP  -> prPrec i 0 (concatD [doc (showString "exists")])
   PathFP flowre -> prPrec i 0 (concatD [prt 0 flowre])


instance Print FlowRE where
  prt i e = case e of
   ConsF flowre0 connre flowre -> prPrec i 0 (concatD [prt 0 flowre0 , prt 0 connre , prt 0 flowre])
   AnyFRE  -> prPrec i 0 (concatD [doc (showString ".*")])


instance Print PortDeclarationType where
  prt i e = case e of
   EmptyPDT  -> prPrec i 0 (concatD [])
   PortTypePDT porttypeconstraints -> prPrec i 0 (concatD [doc (showString ":") , doc (showString "{") , prt 0 porttypeconstraints , doc (showString "}")])


instance Print PortDeclarationConnection where
  prt i e = case e of
   EmptyPDC  -> prPrec i 0 (concatD [])
   Connection connection expressions -> prPrec i 0 (concatD [prt 0 connection , prt 0 expressions])


instance Print Expression where
  prt i e = case e of
   IntExpression n -> prPrec i 0 (concatD [prt 0 n])
   StringExpression str -> prPrec i 0 (concatD [prt 0 str])
   DirectionExpression direction -> prPrec i 0 (concatD [prt 0 direction])
   PositionExpression position -> prPrec i 0 (concatD [prt 0 position])
   QualNameExpression qualname -> prPrec i 0 (concatD [prt 0 qualname])
   ParenExpression expression -> prPrec i 0 (concatD [doc (showString "(") , prt 0 expression , doc (showString ")")])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Direction where
  prt i e = case e of
   InputDirection  -> prPrec i 0 (concatD [doc (showString "input")])
   OutputDirection  -> prPrec i 0 (concatD [doc (showString "output")])
   BidirectionalDirection  -> prPrec i 0 (concatD [doc (showString "bidirectional")])


instance Print Position where
  prt i e = case e of
   SubjectPosition  -> prPrec i 0 (concatD [doc (showString "subject")])
   ObjectPosition  -> prPrec i 0 (concatD [doc (showString "object")])


instance Print QualName where
  prt i e = case e of
   UnQual name -> prPrec i 0 (concatD [prt 0 name])
   Qual qualname name -> prPrec i 0 (concatD [prt 0 qualname , doc (showString ".") , prt 0 name])


instance Print Name where
  prt i e = case e of
   TypeIdent classid -> prPrec i 0 (concatD [prt 0 classid])
   Ident identifier -> prPrec i 0 (concatD [prt 0 identifier])


instance Print PortTypeConstraint where
  prt i e = case e of
   PortTypeConstraint flowid noneexpression -> prPrec i 0 (concatD [prt 0 flowid , doc (showString "=") , prt 0 noneexpression])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print NoneExpression where
  prt i e = case e of
   NoneE  -> prPrec i 0 (concatD [doc (showString "*")])
   SomeE expression -> prPrec i 0 (concatD [prt 0 expression])


instance Print Connection where
  prt i e = case e of
   BidirectionalConnection  -> prPrec i 0 (concatD [doc (showString "<-->")])
   LeftToRightConnection  -> prPrec i 0 (concatD [doc (showString "-->")])
   RightToLeftConnection  -> prPrec i 0 (concatD [doc (showString "<--")])
   NeutralConnection  -> prPrec i 0 (concatD [doc (showString "--")])


instance Print Identifier where
  prt i e = case e of
   Identifier lident -> prPrec i 0 (concatD [prt 0 lident])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print PortId where
  prt i e = case e of
   PortId lident -> prPrec i 0 (concatD [prt 0 lident])


instance Print FlowId where
  prt i e = case e of
   FlowId lident -> prPrec i 0 (concatD [prt 0 lident])


instance Print ClassId where
  prt i e = case e of
   ClassId uident -> prPrec i 0 (concatD [prt 0 uident])



