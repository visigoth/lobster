{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
--
-- Export.hs --- Compiling Lobster to SELinux policy.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reesrved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

module Lobster.SELinux.Export (
  exportSELinux
  ) where

import Control.Applicative
import Control.Error (hush)
import Control.Lens
import Data.List (find)
import Data.Maybe (isJust, catMaybes, fromMaybe)
import Data.Text (Text)

import Lobster.Core
import Text.PrettyPrint.Mainland   as PP

import qualified Data.Map          as M
import qualified Data.Set          as S
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO

----------------------------------------------------------------------
-- Utilities

expStringSEName :: Fold (Exp l) SEName
expStringSEName = _ExpString . to getLitString . to SEName

----------------------------------------------------------------------
-- Simple SELinux AST for Pretty Printing

-- | A symbolic identifier.
newtype SEName = SEName Text
  deriving (Eq, Ord, Show)

-- | Return a domain's name as an 'SEName'.
domSEName :: Domain l -> SEName
domSEName = SEName . view domainName

-- | Return a port's name as an 'SEName'.
portSEName :: Port l -> SEName
portSEName = SEName . view portName

-- | Information about an SELinux allow rule.
data Allow = Allow !SEName !SEName !SEName !(S.Set SEName) (Maybe SEBoolExp)
  deriving (Eq, Ord, Show)

-- | An SELinux boolean expression.
data SEBoolExp
  = SEBoolVar  !SEName
  | SEBoolLit  Bool
  | SENot      SEBoolExp
  | SEAnd      SEBoolExp SEBoolExp
  | SEOr       SEBoolExp SEBoolExp
  | SEEqual    SEBoolExp SEBoolExp
  | SENotEqual SEBoolExp SEBoolExp
  deriving (Eq, Ord, Show)

-- | Convert a Lobster expression to an SELinux boolean expression.
expSE :: Exp l -> Maybe SEBoolExp
expSE e =
  case e of
    ExpBool (LitBool _ b)                -> return (SEBoolLit b)
    ExpVar (Unqualified (VarName _ n))   -> return (SEBoolVar (SEName n))
    ExpVar (Qualified _ _ _)             -> error "Cannot convert qualified variable"
    ExpUnaryOp  _    UnaryOpNot       e  -> SENot <$> expSE e
    ExpBinaryOp _ e1 BinaryOpAnd      e2 -> SEAnd <$> expSE e1 <*> expSE e2
    ExpBinaryOp _ e1 BinaryOpOr       e2 -> SEOr  <$> expSE e1 <*> expSE e2
    ExpBinaryOp _ e1 BinaryOpEqual    e2 -> SEEqual <$> expSE e1 <*> expSE e2
    ExpBinaryOp _ e1 BinaryOpNotEqual e2 -> SENotEqual <$> expSE e1 <*> expSE e2
    _                                    -> Nothing

-- | A statement in an SELinux policy.  We generate a limited set
-- of forms of the actual allowed syntax for simplicity.
data SEStmt
  = SEAttr     !SEName
  | SEType     !SEName
  | SETypeAttr !SEName !SEName
  | SEAllow    !Allow
  | SETrans    !SEName !SEName !SEName !SEName
  | SECall     !SEName [SEName]   -- m4 macro call
  | SEMisc     !SEName (Annotation ())

instance Pretty SEName where
  ppr (SEName x) = fromText x

-- XXX not worrying about eliminating unnecessary parentheses
-- here for now using fixity/associativity...
instance Pretty SEBoolExp where
  ppr (SEBoolVar name)   = ppr name
  ppr (SEBoolLit True)   = text "true"
  ppr (SEBoolLit False)  = text "false"
  ppr (SENot e)          = parens (text "!" <> parens (ppr e))
  ppr (SEAnd e1 e2)      = parens (ppr e1 <+> text "&&" <+> ppr e2)
  ppr (SEOr e1 e2)       = parens (ppr e1 <+> text "||" <+> ppr e2)
  ppr (SEEqual e1 e2)    = parens (ppr e1 <+> text "==" <+> ppr e2)
  ppr (SENotEqual e1 e2) = parens (ppr e1 <+> text "!=" <+> ppr e2)

braceList :: [Exp a] -> Doc
braceList []  = lbrace <> rbrace
braceList [x] = pprId x
braceList xs  = lbrace <+> (sep $ map pprId xs) <+> rbrace

-- | Pretty print an identifier, stripping quotes off of escaped string
-- literals. Used for "SEMisc" annotations.
pprId :: Exp a -> Doc
pprId (ExpString (LitString _ x)) = PP.fromText x
pprId expr = ppr expr

instance Pretty SEStmt where
  ppr (SEAttr name) =
    text "attribute" <+> ppr name <> semi
  ppr (SEType name) =
    text "type" <+> ppr name <> semi
  ppr (SETypeAttr ty attr) =
    text "typeattribute" <+> ppr ty <+> ppr attr <> semi
  ppr (SEAllow (Allow subj obj cls perms (Just e))) =
    text "if" <+> parens (ppr e) <+> lbrace
              </> indent 2 (ppr (SEAllow (Allow subj obj cls perms Nothing)))
              </> rbrace
  ppr (SEAllow (Allow subj obj cls perms Nothing)) =
    text "allow" <+> ppr subj <+> ppr obj <+> colon <+> ppr cls
                 <+> lbrace <+> (sep $ map ppr $ S.toList perms) <+> rbrace
                 <>  semi
  ppr (SETrans subj obj cls new) =
    text "type_transition" <+> ppr subj <+> ppr obj <+> colon <+> ppr cls
                           <+> ppr new <> semi
  ppr (SECall name args) =
    ppr name <> parens (commasep $ map ppr args)

  ppr (SEMisc (SEName ty) ann)
    | ty == "role"
    , Just [name] <- lookupAnnotation "Name" ann
    , Just types <- lookupAnnotation "Types" ann
      = if null types
        then text "role" <+> pprId name <> semi
        else text "role" <+> pprId name <+> braceList types <> semi
    | ty == "attribute_role"
    , Just [ExpVar (Unqualified (VarName _ name))] <- lookupAnnotation "Name" ann
      = text "attribute_role" <+> ppr (SEName name) <> semi
    | ty == "role_attribute"
    , Just [ExpVar (Unqualified (VarName _ name))] <- lookupAnnotation "Role" ann
    , Just attributes <- lookupAnnotation "Attributes" ann
      = text "roleattribute" <+> ppr (SEName name)
                             <+> (commasep $ map pprId attributes)
                             <>  semi
    | ty == "type_alias"
    , Just [ExpVar (Unqualified (VarName _ name))] <- lookupAnnotation "Name" ann
    , Just aliases <- lookupAnnotation "Aliases" ann
      = text "typealias" <+> ppr (SEName name) <+> braceList aliases <> semi
    | ty == "role_transition"
    , Just currentRoles <- lookupAnnotation "CurrentRoles" ann
    , Just types <- lookupAnnotation "Types" ann
    , Just [newRole] <- lookupAnnotation "NewRole" ann
      = text "role_transition" <+> braceList currentRoles
                               <+> braceList types
                               <+> pprId newRole
                               <>  semi
    | ty == "role_allow"
      , Just fromRoles <- lookupAnnotation "FromRole" ann
      , Just toRoles <- lookupAnnotation "ToRole" ann
        = text "allow" <+> braceList fromRoles
                       <+> braceList toRoles
                       <>  semi
    | otherwise = PP.empty

----------------------------------------------------------------------
-- Cross-Module Port Lookups

-- | Look up a subdomain by name.
lookupSubdomain :: Module l -> Domain l -> Qualified VarName l -> Maybe (Domain l)
lookupSubdomain m dom (Unqualified (VarName _ name)) =
  find ((name ==) . view domainName) (map getDom subdoms)
  where
    getDom x = view (idDomain x) m
    subdoms = S.toList (dom ^. domainSubdomains)
lookupSubdomain m dom qualname = do
  let name = getVarName (getUnqualified qualname)
  root <- m ^? moduleEnv
  env <- hush (lookupEnv m qualname root)
  (subid, _) <- M.lookup name (env ^. envSubdomains)
  return (m ^. idDomain subid)

-- | Look up a domain port by name.
lookupPort :: Module l -> Domain l -> Qualified VarName l -> Maybe (Port l)
lookupPort m dom (Unqualified (VarName _ name)) =
  find ((name ==) . view portName) (map getPort ports)
  where
    getPort x = view (idPort x) m
    ports = S.toList (dom ^. domainPorts)
lookupPort m dom qualname = do
  let name = getVarName (getUnqualified qualname)
  root <- m ^? moduleEnv
  env <- hush (lookupEnv m qualname root)
  portId <- M.lookup name (env ^. envPorts)
  return (m ^. idPort portId)

-- | Follow annotations across a cross module port.
crossModulePort :: Module l
                -> Connection l
                -> Lens' (Connection l) PortId
                -> Text
                -> Maybe PortId
crossModulePort m conn l t =
  let pid  = view l conn
      port = m ^. idPort pid
      dom1 = m ^. idDomain (port ^. portDomain)
      name = port ^. portName in
  if portIsModule name
     then
       case lookupAnnotation t (conn ^. connectionAnnotation) of
              Just [ ExpVar dom2
                   , ExpVar port2] -> do
                dom3  <- lookupSubdomain m dom1 dom2
                port3 <- lookupPort      m dom3 port2
                return (port3 ^. portId)
              _ -> Nothing
     else Just pid  -- regular connection

-- | Return the left port of a connection, accounting for
-- cross module ports.
leftPort :: Module l -> Connection l -> PortId
leftPort m conn =
  fromMaybe (conn ^. connectionLeft) (crossModulePort m conn connectionLeft "Lhs")

-- | Return the right port of a connection, accounting for
-- cross module ports.
rightPort :: Module l -> Connection l -> PortId
rightPort m conn =
  fromMaybe (conn ^. connectionRight) (crossModulePort m conn connectionRight "Rhs")

----------------------------------------------------------------------
-- Object Utilities

-- | Return true if a domain has a specific annotation.
domainHasAnnotation :: Text -> Domain l -> Bool
domainHasAnnotation s dom =
  isJust $ lookupAnnotation s (dom ^. domainAnnotation)

-- | Return true if a domain is an SELinux type.
domainIsType :: Domain l -> Bool
domainIsType = domainHasAnnotation "Type"

-- | Return true if a domain is an SELinux attribute.
domainIsAttr :: Domain l -> Bool
domainIsAttr = domainHasAnnotation "Attribute"

-- | Return true if a domain is a type or attribute.
domainIsTypeOrAttr :: Domain l -> Bool
domainIsTypeOrAttr dom = domainIsType dom || domainIsAttr dom

-- | Return the statement type name if a domain contains an
-- annotation for a misc. SELinux statement.
domainIsMiscStmt :: Domain l -> Maybe SEStmt
domainIsMiscStmt dom =
  let anns = fmap (const ()) (dom ^. domainAnnotation) in
    case lookupAnnotation "SysDomain" anns of
    Just [ExpVar (Unqualified (VarName _ name))] -> Just (SEMisc (SEName name) anns)
    _ -> Nothing

-- | Return true if a domain is a macro instantiation.
domainIsMacro :: Domain l -> Maybe (SEName, [SEName])
domainIsMacro dom =
  case lookupAnnotation "Macro" (dom ^. domainAnnotation) of
    Just exprs -> do
      let name = dom ^. domainClassName . to T.toLower
      return (SEName name, exprs ^.. folded . expStringSEName)
    Nothing    -> Nothing

-- | Return true if a port name is an attribute membership port.
portIsMember :: Text -> Bool
portIsMember s
  | s == "member_subj" = True
  | s == "member_obj"  = True
  | otherwise          = False

-- | Return true if a port name is an attribute port.
portIsAttr :: Text -> Bool
portIsAttr s
  | s == "attribute_subj" = True
  | s == "attribute_obj"  = True
  | otherwise             = False

-- | Return true if a port name is a cross-module port.
portIsModule :: Text -> Bool
portIsModule s
  | s == "module_subj" = True
  | s == "module_obj"  = True
  | otherwise          = False

-- | Return the type and attribute domains for a connection if it
-- represents attribute membership.
connIsMembership :: Module l -> Connection l -> Maybe (Domain l, Domain l)
connIsMembership m conn =
  let portL = m ^. idPort (leftPort m conn)
      nameL = portL ^. portName
      domL  = m ^. idDomain (portL ^. portDomain)
      portR = m ^. idPort (rightPort m conn)
      nameR = portR ^. portName
      domR  = m ^. idDomain (portR ^. portDomain) in
  if | portIsMember nameL && portIsAttr nameR ->
       Just (domL, domR)
     | portIsMember nameR && portIsAttr nameL ->
       Just (domR, domL)
     | otherwise ->
       Nothing

-- | Return the set of permissions from a connection.
connPerms :: Connection l -> S.Set SEName
connPerms conn = S.fromList (anns ^.. folded . folded . expStringSEName)
  where
    anns = lookupAnnotations "Perm" (conn ^. connectionAnnotation)

-- | Return the conditional expression for a connection if any.
connConditional :: Connection l -> Maybe SEBoolExp
connConditional conn =
  case lookupAnnotation "CondExpr" (conn ^. connectionAnnotation) of
    Just (e:[]) -> expSE e
    _           -> Nothing

-- | Return the subject, object, class, and permissions for a
-- connection if it represents an allow rule.
connIsAllow :: Module l -> Connection l -> Maybe Allow
connIsAllow m conn =
  let portL = m ^. idPort (leftPort m conn)
      domL  = m ^. idDomain (portL ^. portDomain)
      portR = m ^. idPort (rightPort m conn)
      domR  = m ^. idDomain (portR ^. portDomain)
      cond  = connConditional conn
      perms = connPerms conn in
    if | domainIsTypeOrAttr domL && domainIsTypeOrAttr domR && not (S.null perms) ->
         case (portL ^. portPosition, portR ^. portPosition) of
           (PosSubject, PosObject) ->
             Just (Allow (domSEName domL) (domSEName domR)
                         (portSEName portR) perms cond)
           (PosObject, PosSubject) ->
             Just (Allow (domSEName domR) (domSEName domL)
                         (portSEName portL) perms cond)
           _ -> Nothing
       | otherwise -> Nothing

----------------------------------------------------------------------
-- Module Queries

-- | Return all SELinux type domains in a module.
moduleTypes :: DomainTree l -> [Domain l]
moduleTypes dt = filter (domainIsType) (allDomains dt)

-- | Return all SELinux attributes in a module.
moduleAttrs :: DomainTree l -> [Domain l]
moduleAttrs dt = filter (domainIsAttr) (allDomains dt)

-- | Return SELinux misc. statements for a module.
moduleMiscStmts :: DomainTree l -> [SEStmt]
moduleMiscStmts dt = catMaybes $ map domainIsMiscStmt (allDomains dt)

-- | Return all attribute memberships in a module.
--
-- XXX I'm not sure this does the right thing for subdomains.
moduleTypeAttrs :: Module l -> [(Domain l, Domain l)]
moduleTypeAttrs m =
  catMaybes $ map (connIsMembership m) (M.elems (m ^. moduleConnections))

-- | Return all allow rules in a module.
moduleAllows :: Module l -> [Allow]
moduleAllows m =
  catMaybes $ map (connIsAllow m) (M.elems (m ^. moduleConnections))

-- | Return all macro call statements in a module.
moduleMacros :: Module l -> [(SEName, [SEName])]
moduleMacros m =
  catMaybes $ map domainIsMacro (M.elems (m ^. moduleDomains))

----------------------------------------------------------------------
-- Statement Generation

-- | Return type declarations for a domain tree.
moduleTypeDecls :: DomainTree l -> [SEStmt]
moduleTypeDecls = map (SEType . domSEName) . moduleTypes

-- | Return attribute declarations for a domain tree.
moduleAttrDecls :: DomainTree l -> [SEStmt]
moduleAttrDecls = map (SEAttr . domSEName) . moduleAttrs

-- | Return attribute membership declarations.
moduleTypeAttrDecls :: Module l -> [SEStmt]
moduleTypeAttrDecls m =
  [ SETypeAttr (domSEName ty) (domSEName attr)
  | (ty, attr) <- moduleTypeAttrs m
  ]

-- | Return allow declarations for a module.
moduleAllowDecls :: Module l -> [SEStmt]
moduleAllowDecls = map SEAllow . moduleAllows

-- | Return macro calls for a module.
moduleCalls :: Module l -> [SEStmt]
moduleCalls m = [SECall f args | (f, args) <- moduleMacros m]

-- | Return a list of statements for a module.
--
-- TODO: Consider returning a data type here so we can pretty
-- print it more nicely.
moduleDecls :: Module l -> [SEStmt]
moduleDecls m = types ++ attrs ++ typeAttrs ++ allows ++ calls ++ misc
  where
    dtree = domainTree m
    types = moduleTypeDecls dtree
    attrs = moduleAttrDecls dtree
    typeAttrs = moduleTypeAttrDecls m
    allows = moduleAllowDecls m
    calls = moduleCalls m
    misc = moduleMiscStmts dtree

-- | Export a Lobster module to an SELinux policy file as lazy text.
exportSELinux :: Module l -> TL.Text
exportSELinux m = prettyLazyText 120 doc
  where
    doc = stack $ map ppr $ moduleDecls m
