{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
--
-- Eval.hs --- Lobster to graph evaluator.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

module Lobster.Core.Eval
  ( -- * Modules
    Module()
  , moduleDomains
  , modulePorts
  , moduleConnections
  , moduleRootDomain
  , idDomain
  , pathDomain
  , idPort
  , idConnection
  , isntNegativeConn

    -- * Evaluation
  , evalPolicy

    -- * Domains
  , Domain()
  , DomainId(..)
  , domainId
  , domainName
  , domainClassName
  , domainPath
  , domainSubdomains
  , domainParent
  , domainPorts
  , domainLabel
  , domainAnnotation
  , domainClassAnnotation
  , domainIsExplicit
  , domainNegativeConns

    -- * Ports
  , Port()
  , PortId(..)
  , portId
  , portName
  , portPath
  , portPosition
  , portDirection
  , portLabel
  , portAnnotation
  , portDomain
 
    -- * Connections
  , ConnLevel(..)
  , ConnectionId(..)
  , Connection()
  , connectionId
  , connectionLeft
  , connectionRight
  , connectionLevel
  , connectionType
  , connectionLabel
  , connectionAnnotation
  , revConn
  , revLevel
  ) where

import Control.Applicative ((<$>))
import Control.Error
import Control.Lens hiding (op)
import Control.Monad (unless)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Monoid ((<>), mempty)
import Data.Text (Text)

import Lobster.Core.Util
import Lobster.Core.Error

import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Lobster.Core.AST     as A

----------------------------------------------------------------------
-- Environments

-- | A class definition.
data Class l = Class
  { _className        :: A.TypeName l
  , _classPath        :: Text
  , _classArgs        :: [A.VarName l]
  , _classBody        :: [A.Stmt l]
  , _classAnnotation  :: A.Annotation l
  , _classIsExplicit  :: Bool
  } deriving (Show, Functor)

-- | A domain's environment.
data Env l = Env
  { _envClasses      :: M.Map Text (Class l)
  , _envPorts        :: M.Map Text PortId
  , _envSubdomains   :: M.Map Text (DomainId, Env l)
  , _envVars         :: M.Map Text (Value l)
  } deriving (Show, Functor)

-- | The initial top level environment.
initialEnv :: Env l
initialEnv = Env
  { _envClasses    = M.empty
  , _envPorts      = M.empty
  , _envSubdomains = M.empty
  , _envVars       = M.empty
  }

----------------------------------------------------------------------
-- Graph Data Types

-- | The value of an evaluated expression.
data Value l
  = ValueInt l Integer
  | ValueString l Text
  | ValueBool l Bool
  | ValueDirection l A.Direction
  | ValuePosition l A.Position
  deriving (Show, Functor)

instance A.Labeled Value where
  label (ValueInt l _)       = l
  label (ValueString l _)    = l
  label (ValueBool l _)      = l
  label (ValueDirection l _) = l
  label (ValuePosition l _)  = l

newtype PortId = PortId { getPortId :: Int }
  deriving (Eq, Ord, Show)

-- | A port definition.
data Port l = Port
  { _portId         :: PortId
  , _portName       :: Text
  , _portPath       :: Text
  , _portPosition   :: A.Position
  , _portDirection  :: Maybe A.Direction
  , _portLabel      :: l
  , _portAnnotation :: A.Annotation l
  , _portDomain     :: DomainId
  } deriving (Show, Functor)

instance A.Labeled Port where
  label = _portLabel

newtype DomainId = DomainId { getDomainId :: Int }
  deriving (Eq, Ord, Show)

-- | A 'negative connection' within a domain.  For implicit
-- domains, this prohibits an internal connection between
-- the two ports.  For explicit domains, this has no effect.
--
-- TODO: We might want to signal an error if there are
-- conflicting positive and negative internal connections.
data NegativeConn = NegativeConn
  { _negativeConnLeft       :: !PortId
  , _negativeConnRight      :: !PortId
  } deriving (Eq, Show, Ord)

makeLenses ''NegativeConn

-- | A domain definition.
data Domain l = Domain
  { _domainId               :: DomainId
  , _domainName             :: Text
  , _domainClassName        :: Text
  , _domainPath             :: Text
  , _domainSubdomains       :: S.Set DomainId
  , _domainParent           :: Maybe DomainId
  , _domainPorts            :: S.Set PortId
  , _domainLabel            :: l
  , _domainAnnotation       :: A.Annotation l
  , _domainClassAnnotation  :: A.Annotation l
  , _domainIsExplicit       :: Bool
  , _domainNegativeConns    :: S.Set NegativeConn
  } deriving (Show, Functor)

instance A.Labeled Domain where
  label = _domainLabel

-- | The initial top-level domain.
topDomain :: l -> DomainId -> Domain l
topDomain l domId = Domain
  { _domainId               = domId
  , _domainName             = "System"
  , _domainClassName        = ""
  , _domainPath             = ""
  , _domainSubdomains       = S.empty
  , _domainParent           = Nothing
  , _domainPorts            = S.empty
  , _domainLabel            = l
  , _domainAnnotation       = mempty
  , _domainClassAnnotation  = mempty
  , _domainIsExplicit       = False
  , _domainNegativeConns    = S.empty
  }

-- | Relationship between the left and right domains of a
-- port connection.
data ConnLevel = ConnLevelPeer      -- ports at same level
               | ConnLevelParent    -- right port is in subdomain
               | ConnLevelChild     -- left port is in subdomain
               | ConnLevelInternal  -- both ports in same domain
  deriving (Eq, Ord, Show)

-- | A unique identifier for a connection as parsed from the
-- Lobster source.
newtype ConnectionId = ConnectionId { getConnectionId :: Int }
  deriving (Eq, Ord, Show)

-- | Graph edge label type.
data Connection l = Connection
  { _connectionId         :: !ConnectionId
  , _connectionLeft       :: !PortId
  , _connectionRight      :: !PortId
  , _connectionLevel      :: !ConnLevel
  , _connectionType       :: !A.ConnType
  , _connectionLabel      :: !l
  , _connectionAnnotation :: !(A.Annotation l)
  } deriving (Eq, Ord, Show, Functor)

{-
-- | Compare connections for equality, ignoring their label.
instance Eq l => Eq (Connection l) where
  (==) a b =
    _connectionLeft       a == _connectionLeft       b &&
    _connectionRight      a == _connectionRight      b &&
    _connectionLevel      a == _connectionLevel      b &&
    _connectionType       a == _connectionType       b &&
    _connectionAnnotation a == _connectionAnnotation b

-- | Compare connections, ignoring their label.
instance Ord l => Ord (Connection l) where
  compare a b =
    compare (_connectionLeft       a) (_connectionLeft       b) <>
    compare (_connectionRight      a) (_connectionRight      b) <>
    compare (_connectionLevel      a) (_connectionLevel      b) <>
    compare (_connectionType       a) (_connectionType       b) <>
    compare (_connectionAnnotation a) (_connectionAnnotation b)
-}

instance A.Labeled Connection where
  label = _connectionLabel

-- type Graph l = G.Gr () (Connection l)

data Module l = Module
  { _moduleDomains          :: M.Map DomainId (Domain l)
  , _modulePorts            :: M.Map PortId (Port l)
  , _moduleConnections      :: M.Map ConnectionId (Connection l)
  , _moduleRootDomain       :: DomainId
  , _moduleEnv              :: Env l
  , _moduleNextDomainId     :: Int
  , _moduleNextPortId       :: Int
  , _moduleNextConnectionId :: Int
  } deriving Show

emptyModule :: l -> Module l
emptyModule l = Module
  { _moduleDomains          = M.singleton (DomainId 0) (topDomain l (DomainId 0))
  , _modulePorts            = M.empty
  , _moduleConnections      = M.empty
  , _moduleRootDomain       = (DomainId 0)
  , _moduleEnv              = initialEnv
  , _moduleNextDomainId     = 1
  , _moduleNextPortId       = 0
  , _moduleNextConnectionId = 0
  }

makePrisms ''Value

makeLenses ''Env
makeLenses ''Class
makeLenses ''Port
makeLenses ''Domain
makeLenses ''Module
makeLenses ''Connection

-- | Reverse a connection level if a parent/child type.
revLevel :: ConnLevel -> ConnLevel
revLevel ConnLevelParent   = ConnLevelChild
revLevel ConnLevelChild    = ConnLevelParent
revLevel ConnLevelPeer     = ConnLevelPeer
revLevel ConnLevelInternal = ConnLevelInternal

-- | Reverse annotations for a connection.  This switches "Lhs"
-- to "Rhs" and vice versa.
revAnnotation :: A.Annotation l -> A.Annotation l
revAnnotation (A.Annotation xs) = A.Annotation (map go xs)
  where
    go (ty@(A.TypeName l name), args)
      | name == "Lhs" = (A.TypeName l "Rhs", args)
      | name == "Rhs" = (A.TypeName l "Lhs", args)
      | otherwise     = (ty,                 args)

-- | Reverse a connection.
revConn :: Connection l -> Connection l
revConn conn = (`execState` conn) $ do
  portL                <- use connectionLeft
  portR                <- use connectionRight
  connectionLevel      %= revLevel
  connectionType       %= A.revConnType
  connectionAnnotation %= revAnnotation
  connectionLeft       .= portR
  connectionRight      .= portL

-- | A partial lens for a domain by ID in a module.
idDomain :: DomainId -> Lens' (Module l) (Domain l)
idDomain domId = singular (moduleDomains . ix domId)

-- | A partial lens for a port by ID in a module.
idPort :: PortId -> Lens' (Module l) (Port l)
idPort pid = singular (modulePorts . ix pid)

-- | A partial lens for a connection by ID.
idConnection :: ConnectionId -> Lens' (Module l) (Connection l)
idConnection connId = singular (moduleConnections . ix connId)

-- | Look up a domain by path name.
pathDomain :: Module l -> Text -> Maybe (Domain l)
pathDomain m t =
  case go (m ^. moduleRootDomain) (m ^. moduleEnv) (T.split (== '.') t) of
    Just domId -> Just $ m ^. idDomain domId
    Nothing    -> Nothing
  where
    go domId _    [] = Just domId
    go _     env (x:xs) =
      case M.lookup x (env ^. envSubdomains) of
        Just (subDomId, subEnv) ->
          go subDomId subEnv xs
        Nothing -> Nothing

{-
-- test function to relabel the graph for use with graphviz
labelledGraph :: Module l -> G.Gr Text Text
labelledGraph m = G.undir (G.emap goE (G.gmap goN (m ^. moduleGraph)))
  where
    {- this is too spammy for big graphs
    getName :: PortId -> Text
    getName p = m ^?! modulePorts . ix p . portName
    goE conn =
      let nameL = getName $ conn ^. connectionLeft
          nameR = getName $ conn ^. connectionRight in
        nameL <> " -- " <> nameR
    -}
    goE = const ""
    goN (preds, node, _, posts) =
      let name = m ^?! moduleDomains . ix (DomainId node) . domainPath in
      (preds, node, name, posts)
-}

----------------------------------------------------------------------
-- Evaluator Monad

-- | State and error handling monad for evaluation.
type Eval l a = StateT (Module l) (Either (Error l)) a

-- | Throw an error in the 'Eval' monad.
lose :: Error l -> Eval l a
lose = lift . throwE

-- | Throw an error if a 'Maybe' value is Nothing.
maybeLose :: Error l -> Maybe a -> Eval l a
maybeLose e x = lift $ note e x

----------------------------------------------------------------------
-- Evaluation

-- | Add a class definition to the current environment.
addClass :: A.TypeName l -> Class l -> Eval l ()
addClass (A.TypeName _ name) cl = do
  moduleEnv . envClasses . at name ?= cl

-- | Look up a class definition.
lookupClass :: A.TypeName l -> Eval l (Class l)
lookupClass (A.TypeName l name) = do
  x <- use (moduleEnv . envClasses . at name)
  maybeLose (UndefinedClass l name) x

-- | Look up a variable name in the current domain.  Raises an
-- 'UndefinedVar' error if it is not found.
lookupVar :: A.VarName l -> Eval l (Value l)
lookupVar (A.VarName l name) = do
  x <- use (moduleEnv . envVars . at name)
  maybeLose (UndefinedVar l name) x

-- | Convert a port name to a string for error messages.
fullPortName :: A.PortName l -> Text
fullPortName (A.UPortName (A.VarName _ name)) = name
fullPortName (A.QPortName _ (A.VarName _ n1) (A.VarName _ n2)) = n1 <> "." <> n2

-- | Resolve a port in the current domain, returning its port
-- id if it is valid.
lookupPort :: A.PortName l -> Eval l PortId
lookupPort (A.UPortName (A.VarName l name)) = do
  port  <- use (moduleEnv . envPorts . at name)
  port' <- maybeLose (UndefinedPort l name) port
  return port'
lookupPort pid@(A.QPortName _ (A.VarName l1 domN) (A.VarName l2 portN)) = do
  -- look up subdomain, get domain id and subdomain environment
  x <- use (moduleEnv . envSubdomains . at domN)
  (_, subEnv) <- maybeLose (UndefinedDomain l1 domN) x
  -- look up port in subdomain environment
  let y = subEnv ^. envPorts . at portN
  port <- maybeLose (UndefinedPort l2 (fullPortName pid)) y
  return port

-- | Add a port definition to the current graph.
addPort :: Port l -> Eval l PortId
addPort port = do
  pid <- PortId <$> (moduleNextPortId <<+= 1)
  modulePorts . at pid ?= port
  -- add port to current root domain
  rootId <- use moduleRootDomain
  moduleDomains . ix rootId . domainPorts . contains pid .= True
  return pid

-- | Get a port by ID.
getPort :: PortId -> Eval l (Port l)
getPort pid = do
  ports <- use modulePorts
  case ports ^? ix pid of
    Just port -> return port
    Nothing   -> lose $ MiscError "internal error: undefined port"

-- | Add a subdomain definition to the current graph.
addDomain :: Domain l -> Eval l DomainId
addDomain dom = do
  domId <- DomainId <$> (moduleNextDomainId <<+= 1)
  moduleDomains . at domId ?= dom
  -- add domain as subdomain of current root
  rootId <- use moduleRootDomain
  moduleDomains . ix rootId . domainSubdomains . contains domId .= True
  return domId

-- | Get a domain by ID.
getDomain :: DomainId -> Eval l (Domain l)
getDomain domId = do
  domains <- use moduleDomains
  case domains ^? ix domId of
    Just dom -> return dom
    Nothing  -> lose $ MiscError "internal error: undefined domain"

-- | Return true if one domain is a subdomain of the other.
isSubdomain :: DomainId -> DomainId -> Eval l Bool
isSubdomain parentId childId = do
  parent <- getDomain parentId
  return $ S.member childId (parent ^. domainSubdomains)

-- | Return true if two domains have the same parent.
isPeerDomain :: DomainId -> DomainId -> Eval l Bool
isPeerDomain domId1 domId2 = do
  dom1 <- getDomain domId1
  dom2 <- getDomain domId2
  return (dom1 ^. domainParent == dom2 ^. domainParent)

-- | Get the connection level between two ports.  This raises
-- an error if the ports cannot be connected because they are
-- not peers or from a domain to a port in a subdomain.
connLevel :: A.PortName l -> PortId -> A.PortName l -> PortId -> Eval l ConnLevel
connLevel pnameL pidL pnameR pidR = do
  portL <- getPort pidL
  portR <- getPort pidR
  let domL = portL ^. portDomain
  let domR = portR ^. portDomain
  isPeer   <- isPeerDomain domL domR
  isParent <- isSubdomain domL domR
  isChild  <- isSubdomain domR domL

  if | domL == domR ->
       -- Internal connections are a bit complex.  We different between
       -- inside and outside self-connection syntatically.  If the
       -- connection is defined inside the domain, it's internal.
       -- Otherwise, treat it like any other domain-to-domain connection.
       case (pnameL, pnameR) of
         (A.UPortName _, A.UPortName _) ->
           return ConnLevelInternal
         (A.QPortName _ _ _, A.QPortName _ _ _) ->
           return ConnLevelPeer
         _ -> lose $ MiscError "internal error: weird self connection"
     | isPeer       -> return ConnLevelPeer
     | isParent     -> return ConnLevelParent
     | isChild      -> return ConnLevelChild
     | otherwise    -> lose $ MiscError "internal error: invalid connection"

{-
-- | Return true if an edge already exists in the graph.
edgeExists :: (Int, Int, Connection l) -> Eval l Bool
edgeExists (nodeL, nodeR, _) = do
  gr <- use moduleGraph
  return $ elem nodeR (G.suc gr nodeL)   -- argh, O(n)

-- | Add a connection (in a single direction) between two ports.
addConnection :: l -> PortId -> PortId -> A.ConnType -> A.Annotation l -> Eval l ()
addConnection l portL portR cty ann = do
  level <- connLevel portL portR
  unless (level == ConnLevelInternal) $ do
    domL  <- (^. portDomain) <$> getPort portL
    domR  <- (^. portDomain) <$> getPort portR
    let conn = Connection
                 { _connectionLeft       = portL
                 , _connectionRight      = portR
                 , _connectionLevel      = level
                 , _connectionType       = cty
                 , _connectionLabel      = l
                 , _connectionAnnotation = ann
                 }
    let edge = (nodeId domL, nodeId domR, conn)
    -- TODO: unify the connections rather than skip duplicate edges
    --       we do this because fgl is very slow with lots of duplicate
    --       edges...
    unlessM (edgeExists edge) $
      moduleGraph %= G.insEdge edge
-}

-- | Add a connection to the current module.
addConnection :: l -> A.PortName l -> A.PortName l -> A.ConnType -> A.Annotation l -> Eval l ()
addConnection l pnameL pnameR cty ann = do
  pidL   <- lookupPort pnameL
  pidR   <- lookupPort pnameR
  level  <- connLevel pnameL pidL pnameR pidR
  connId <- ConnectionId <$> (moduleNextConnectionId <<+= 1)
  let conn = Connection
               { _connectionId         = connId
               , _connectionLeft       = pidL
               , _connectionRight      = pidR
               , _connectionLevel      = level
               , _connectionType       = cty
               , _connectionLabel      = l
               , _connectionAnnotation = ann
               }
  moduleConnections . at connId ?= conn

-- | Add a negative connection to the current module.
addNegativeConn :: PortId -> PortId -> Eval l ()
addNegativeConn pidL pidR = do
  rootId <- use moduleRootDomain
  let nconn = NegativeConn pidL pidR
  moduleDomains . ix rootId . domainNegativeConns . contains nconn .= True
  let nconn2 = NegativeConn pidR pidL
  moduleDomains . ix rootId . domainNegativeConns . contains nconn2 .= True

-- | Check for a negative connection during traversal.
isntNegativeConn :: Module l -> PortId -> PortId -> Bool
isntNegativeConn m pidL pidR =
  let domId = m ^. idPort pidL . portDomain in
  let dom   = m ^. idDomain domId in
  not $ dom ^. domainNegativeConns . contains (NegativeConn pidL pidR)

-- | Create a new environment given a set of class definitions
-- inherited from the parent environment and a set of local variables.
newEnv :: M.Map Text (Class l) -> M.Map Text (Value l) -> Env l
newEnv classes locals = Env
  { _envClasses    = classes
  , _envPorts      = M.empty
  , _envSubdomains = M.empty
  , _envVars       = locals
  }

-- | Create a new, empty domain given its name, path, and
-- class.
newDomain :: l -> Text -> Text -> Class l -> DomainId
          -> DomainId -> A.Annotation l -> Domain l
newDomain l name path cls domId parent ann = Domain
  { _domainId              = domId
  , _domainName            = name
  , _domainPath            = path
  , _domainClassName       = cls ^. className . to A.getTypeName
  , _domainSubdomains      = S.empty
  , _domainParent          = Just parent
  , _domainPorts           = S.empty
  , _domainLabel           = l
  , _domainAnnotation      = ann
  , _domainClassAnnotation = cls ^. classAnnotation
  , _domainIsExplicit      = cls ^. classIsExplicit
  , _domainNegativeConns   = S.empty
  }

-- | Execute an action in a new environment for a domain.
inEnv :: DomainId -> Env l -> Eval l a -> Eval l a
inEnv domId env f = do
  oldDomId <- use moduleRootDomain
  oldEnv   <- use moduleEnv
  moduleRootDomain .= domId
  moduleEnv        .= env
  result <- f
  moduleRootDomain .= oldDomId
  moduleEnv        .= oldEnv
  return result

-- | Return true if a name is bound in the current environment
-- given a lens to the map containing bindings.
isBound :: Lens' (Env l) (M.Map Text a) -> Text -> Eval l Bool
isBound field name = do
  m <- use (moduleEnv . field)
  return $ isJust $ m ^? ix name

ofType :: Value l -> Prism' (Value l) (l, a) -> Text -> Eval l a
ofType val tyPrism text =
  case val ^? tyPrism of
    Just x  -> return (snd x)
    Nothing -> lose $ TypeError (A.label val) text

evalBoolBinaryOp :: l -> (Bool -> Bool -> Bool) -> A.Exp l -> A.Exp l -> Eval l (Value l)
evalBoolBinaryOp l f e1 e2 = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  b1 <- ofType v1 _ValueBool "boolean"
  b2 <- ofType v2 _ValueBool "boolean"
  return $ ValueBool l (f b1 b2)

evalBoolUnaryOp :: l -> (Bool -> Bool) -> A.Exp l -> Eval l (Value l)
evalBoolUnaryOp l f e = do
  v1 <- evalExp e
  b1 <- ofType v1 _ValueBool "boolean"
  return $ ValueBool l (f b1)

evalBinaryOp :: l -> A.BinaryOp -> A.Exp l -> A.Exp l -> Eval l (Value l)
evalBinaryOp l A.BinaryOpAnd e1 e2      = evalBoolBinaryOp l (&&) e1 e2
evalBinaryOp l A.BinaryOpOr e1 e2       = evalBoolBinaryOp l (||) e1 e2
evalBinaryOp l A.BinaryOpEqual e1 e2    = evalBoolBinaryOp l (==) e1 e2
evalBinaryOp l A.BinaryOpNotEqual e1 e2 = evalBoolBinaryOp l (/=) e1 e2

evalUnaryOp :: l -> A.UnaryOp -> A.Exp l -> Eval l (Value l)
evalUnaryOp l A.UnaryOpNot e = evalBoolUnaryOp l not e

-- | Evaluate a Lobster expression.
evalExp :: A.Exp l -> Eval l (Value l)
evalExp e =
  case e of
    A.ExpInt       (A.LitInteger   l x) -> return (ValueInt l x)
    A.ExpString    (A.LitString    l x) -> return (ValueString l x)
    A.ExpBool      (A.LitBool      l x) -> return (ValueBool l x)
    A.ExpDirection (A.LitDirection l x) -> return (ValueDirection l x)
    A.ExpPosition  (A.LitPosition  l x) -> return (ValuePosition l x)
    A.ExpBinaryOp  l e1 op e2           -> evalBinaryOp l op e1 e2
    A.ExpUnaryOp   l op e1              -> evalUnaryOp l op e1
    A.ExpVar       var                  -> lookupVar var
    A.ExpParen _   e2                   -> evalExp e2

-- | Return the domain currently in scope during evaluation.
currentDomain :: Eval l (Domain l)
currentDomain = getDomain =<< use moduleRootDomain

-- | Make a qualified name from two names, omitting the period
-- if the first name is empty.
makePath :: Text -> Text -> Text
makePath t1 t2
  | T.null t1 = t2
  | otherwise = t1 <> "." <> t2

-- | Make a qualified name for a newly created class.
getClassPath :: Text -> Eval l Text
getClassPath name = do
  dom <- currentDomain
  return $ makePath (dom ^. domainClassName) name

-- | Make a qualified name for a newly created domain or port.
getMemberPath :: Text -> Eval l Text
getMemberPath name = do
  dom <- currentDomain
  return $ makePath (dom ^. domainPath) name

-- | Build a map of local variables from a list of argument names
-- and expressions to be evaluated.  Raises an error if the number
-- of arguments is incorrect.
buildLocals :: l -> [A.VarName l] -> [A.Exp l] -> Eval l (M.Map Text (Value l))
buildLocals l vars exps = do
  unless (length vars == length exps) $
    lose $ BadArguments l (length vars)
  vals <- mapM evalExp exps
  let varNames = map A.getVarName vars
  return $ M.fromList $ zip varNames vals

-- | Build a new environment for a subdomain given a class and its
-- arguments.
subdomainEnv :: l -> Class l -> [A.Exp l] -> Eval l (Env l)
subdomainEnv l cl args = do
  let clArgs = cl ^. classArgs
  locals <- buildLocals l clArgs args
  classes <- use (moduleEnv . envClasses)
  return (newEnv classes locals)

-- | Return a unique anonymous class name for a domain.
getAnonClassName :: l -> Text -> Eval l (A.TypeName l)
getAnonClassName l t = do
  domId <- use moduleNextDomainId
  return (A.TypeName l ("Anon" <> T.pack (show domId) <> "#" <> t))

-- | Look up a port position from its port attributes.
getPortPos :: [A.PortAttr l] -> Eval l A.Position
getPortPos [] = return A.PosUnknown
getPortPos (A.PortAttr (A.VarName _ name) x:xs)
  | Just expr <- x
  , name == "position" = do
      val <- evalExp expr
      pos <- ofType val _ValuePosition "position"
      return pos
  | otherwise          = getPortPos xs

-- | Evaluate a statement and build up the graph.
evalStmt :: A.Annotation l -> A.Stmt l -> Eval l ()
evalStmt ann (A.StmtPortDecl l (A.VarName _ name) attrs) = do
  -- check for duplicate port definition
  whenM (isBound envPorts name) (lose $ DuplicatePort l name)
  -- create new port and add to graph
  path <- getMemberPath name
  domId <- use moduleRootDomain
  pid <- PortId <$> use moduleNextPortId
  pos <- getPortPos attrs
  let port = Port pid name path pos Nothing l ann domId
  -- XXX ignoring port attributes for now
  _ <- addPort port
  moduleEnv . envPorts . at name ?= pid

evalStmt ann (A.StmtClassDecl l isExp ty@(A.TypeName _ name) args body) = do
  whenM (isBound envClasses name) (lose $ DuplicateClass l name)
  path <- getClassPath name
  let cl = Class ty path args body ann isExp
  addClass ty cl

evalStmt ann (A.StmtDomainDecl l (A.VarName _ name) ty args) = do
  whenM (isBound envSubdomains name) (lose $ DuplicateDomain l name)
  -- look up class and evaluate arguments
  cls    <- lookupClass ty
  env    <- subdomainEnv l cls args
  -- create new domain and add it to the graph
  domPath <- getMemberPath name
  rootId  <- use moduleRootDomain
  -- XXX kind of a hack, get the domain id first before adding it
  domId <- DomainId <$> use moduleNextDomainId
  let dom = newDomain l name domPath cls domId rootId ann
  _ <- addDomain dom
  -- evaluate domain body in new environment
  subEnv <- inEnv domId env $ do
    evalStmts (cls ^. classBody)
    use moduleEnv
  -- add subdomain's environment to our environment
  moduleEnv . envSubdomains . at name ?= (domId, subEnv)

evalStmt ann (A.StmtAnonDomainDecl l isExp var@(A.VarName l2 name) body) = do
  cls <- getAnonClassName l2 name
  evalStmt mempty (A.StmtClassDecl l isExp cls [] body)
  evalStmt ann (A.StmtDomainDecl l var cls [])

evalStmt _ (A.StmtAssign l (A.VarName _ name) e) = do
  whenM (isBound envVars name) (lose $ DuplicateVar l name)
  val <- evalExp e
  moduleEnv . envVars . at name ?= val

-- 'ConnNegative' connections are handled specially.  Rather than
-- adding a connection, add to the set of 'negative connections'
-- for the current domain.  They must also be internal connections
-- (between two ports in the same domain).
evalStmt _ (A.StmtConnection _
              pidL@(A.Qualified _ modsL (A.UPortName _))
              (A.ConnOp _ A.ConnNegative)
              pidR@(A.Qualified _ modsR (A.UPortName _)) = do
  portL <- lookupPort pidL
  portR <- lookupPort pidR
  addNegativeConn portL portR
  addNegativeConn portR portL

evalStmt _ (A.StmtConnection l
              pidL
              (A.ConnOp _ A.ConnNegative)
              pidR) =
  lose $ BadNegativeConn l (fullPortName pidL) (fullPortName pidR)

evalStmt ann (A.StmtConnection l pidL (A.ConnOp _ cty) pidR) = do
  addConnection l pidL pidR cty ann

evalStmt ann1 (A.StmtAnnotation _ ann2 stmt) = evalStmt (ann1 <> ann2) stmt

-- Ignore comments.
--
-- XXX this swallows the annotation, which isn't ideal.  this
-- won't ever happen from parsed Lobster input, but if someone
-- creates Lobster AST manually with an annotation before a
-- comment, it won't affect the next statement.
evalStmt _ (A.StmtComment _ _) = return ()

evalStmts :: [A.Stmt l] -> Eval l ()
evalStmts = mapM_ (evalStmt mempty)

-- | Evaluate a policy and return its graph or an error.
evalPolicy :: A.Policy l -> Either (Error l) (Module l)
evalPolicy (A.Policy l stmts) =
  execStateT (evalStmts stmts) (emptyModule l)
