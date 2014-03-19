{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
--
-- Eval.hs --- Lobster to graph evaluator.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Lobster.Core.Eval
  ( -- * Modules
    Module()
  , moduleDomains
  , modulePorts
  , moduleGraph
  , moduleRootDomain

    -- * Evaluation
  , evalPolicy
  , labelledGraph

    -- * Domains
  , Domain()
  , DomainId(..)
  , domainName
  , domainClassName
  , domainPath
  , domainSubdomains
  , domainPorts
  , domainLabel
  , domainAnnotation
  , domainClassAnnotation

    -- * Domain Trees
  , DomTree(..)
  , moduleDomTree

    -- * Ports
  , Port()
  , PortId(..)
  , portName
  , portPath
  , portPosition
  , portDirection
  , portLabel
  , portAnnotation
  , portDomain
 
    -- * Connections
  , ConnLevel(..)
  , Connection()
  , connectionLeft
  , connectionRight
  , connectionLevel
  , connectionType
  , connectionLabel
  , connectionAnnotation
  ) where

import Control.Applicative ((<$>))
import Control.Error
import Control.Lens hiding (op)
import Control.Monad (unless, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Monoid ((<>), mempty)
import Data.Text (Text)

import Lobster.Core.Error

import qualified Data.Graph.Inductive as G
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Lobster.Core.AST     as A

-- | "when" with a monadic boolean condition.
whenM :: Monad m => m Bool -> m () -> m ()
whenM b f = b >>= (\x -> when x f)

----------------------------------------------------------------------
-- Environments

-- | A class definition.
data Class l = Class
  { _className        :: A.TypeName l
  , _classPath        :: Text
  , _classArgs        :: [A.VarName l]
  , _classBody        :: [A.Stmt l]
  , _classAnnotation  :: A.Annotation l
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

newtype PortId = PortId Int
  deriving (Eq, Ord, Show)

-- | A port definition.
data Port l = Port
  { _portName       :: Text
  , _portPath       :: Text
  , _portPosition   :: Maybe A.Position
  , _portDirection  :: Maybe A.Direction
  , _portLabel      :: l
  , _portAnnotation :: A.Annotation l
  , _portDomain     :: DomainId
  } deriving (Show, Functor)

instance A.Labeled Port where
  label = _portLabel

newtype DomainId = DomainId Int
  deriving (Eq, Ord, Show)

nodeId :: DomainId -> Int
nodeId (DomainId x) = x

-- | A domain definition.
data Domain l = Domain
  { _domainName             :: Text
  , _domainClassName        :: Text
  , _domainPath             :: Text
  , _domainSubdomains       :: S.Set DomainId
  , _domainPorts            :: S.Set PortId
  , _domainLabel            :: l
  , _domainAnnotation       :: A.Annotation l
  , _domainClassAnnotation  :: A.Annotation l
  } deriving (Show, Functor)

instance A.Labeled Domain where
  label = _domainLabel

-- | The initial top-level domain.
topDomain :: l -> Domain l
topDomain l = Domain
  { _domainName             = "System"
  , _domainClassName        = ""
  , _domainPath             = ""
  , _domainSubdomains       = S.empty
  , _domainPorts            = S.empty
  , _domainLabel            = l
  , _domainAnnotation       = mempty
  , _domainClassAnnotation  = mempty
  }

-- | Relationship between the left and right domains of a
-- port connection.
data ConnLevel = ConnLevelPeer      -- ports at same level
               | ConnLevelParent    -- right port is in subdomain
               | ConnLevelChild     -- left port is in subdomain
  deriving (Eq, Ord, Show)

-- | Reverse a connection level if a parent/child type.
revLevel :: ConnLevel -> ConnLevel
revLevel ConnLevelParent = ConnLevelChild
revLevel ConnLevelChild  = ConnLevelParent
revLevel ConnLevelPeer   = ConnLevelPeer

-- | Graph edge label type.
data Connection l = Connection
  { _connectionLeft       :: PortId
  , _connectionRight      :: PortId
  , _connectionLevel      :: ConnLevel
  , _connectionType       :: A.ConnType
  , _connectionLabel      :: l
  , _connectionAnnotation :: A.Annotation l
  } deriving (Eq, Ord, Show, Functor)

instance A.Labeled Connection where
  label = _connectionLabel

type Graph l = G.Gr () (Connection l)

data Module l = Module
  { _moduleDomains      :: M.Map DomainId (Domain l)
  , _modulePorts        :: M.Map PortId (Port l)
  , _moduleGraph        :: Graph l
  , _moduleRootDomain   :: DomainId
  , _moduleEnv          :: Env l
  , _moduleNextDomainId :: Int
  , _moduleNextPortId   :: Int
  } deriving Show

initialModule :: l -> Module l
initialModule l = Module
  { _moduleDomains      = M.singleton (DomainId 0) (topDomain l)
  , _modulePorts        = M.empty
  , _moduleGraph        = G.insNode (0, ()) G.empty
  , _moduleRootDomain   = (DomainId 0)
  , _moduleEnv          = initialEnv
  , _moduleNextDomainId = 1
  , _moduleNextPortId   = 0
  }

makePrisms ''Value

makeLenses ''Env
makeLenses ''Class
makeLenses ''Port
makeLenses ''Domain
makeLenses ''Module
makeLenses ''Connection

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

----------------------------------------------------------------------
-- Domain Trees

-- | A view of the subdomain tree without indirection through IDs.
data DomTree l = DomTree
  { _domTreeDomainId    :: DomainId
  , _domTreeDomain      :: Domain l
  , _domTreeSubdomains  :: [DomTree l]
  , _domTreePorts       :: [Port l]
  } deriving (Show, Functor)

makeLenses ''DomTree

instance Eq (DomTree l) where
  (==) d1 d2 = (d1 ^. domTreeDomainId) == (d2 ^. domTreeDomainId)

instance Ord (DomTree l) where
  compare d1 d2 = compare (d1 ^. domTreeDomainId) (d2 ^. domTreeDomainId)

-- TODO: Could this be better defined as a 'Fold'?
moduleDomTree :: Module l -> DomTree l
moduleDomTree m = domainTree m rootDomId rootDom
  where
    rootDomId = m ^. moduleRootDomain
    rootDom   = m ^?! moduleDomains . ix rootDomId

domainTree :: Module l -> DomainId -> Domain l -> DomTree l
domainTree m domId dom = DomTree
  { _domTreeDomainId    = domId
  , _domTreeDomain      = dom
  , _domTreeSubdomains  = map goSubdomains (S.toList $ dom ^. domainSubdomains)
  , _domTreePorts       = map goPorts      (S.toList $ dom ^. domainPorts)
  }
  where
    goSubdomains subDomId =
      domainTree m subDomId (m ^?! moduleDomains . ix subDomId)
    goPorts portId = m ^?! modulePorts . ix portId

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
lookupPort :: A.PortName l -> Eval l (DomainId, PortId)
lookupPort (A.UPortName (A.VarName l name)) = do
  port  <- use (moduleEnv . envPorts . at name)
  domId <- use moduleRootDomain
  port' <- maybeLose (UndefinedPort l name) port
  return (domId, port')
lookupPort pid@(A.QPortName _ (A.VarName l1 domN) (A.VarName l2 portN)) = do
  -- look up subdomain, get domain id and subdomain environment
  x <- use (moduleEnv . envSubdomains . at domN)
  (domId, subEnv) <- maybeLose (UndefinedDomain l1 domN) x
  -- look up port in subdomain environment
  let y = subEnv ^. envPorts . at portN
  port <- maybeLose (UndefinedPort l2 (fullPortName pid)) y
  return (domId, port)

-- | Add a port definition to the current graph.
addPort :: Port l -> Eval l PortId
addPort port = do
  portId <- PortId <$> (moduleNextPortId <<+= 1)
  modulePorts . at portId ?= port
  -- add port to current root domain
  rootId <- use moduleRootDomain
  moduleDomains . ix rootId . domainPorts . contains portId .= True
  return portId

-- | Add a subdomain definition to the current graph.
addSubdomain :: Domain l -> Eval l DomainId
addSubdomain dom = do
  domId <- DomainId <$> (moduleNextDomainId <<+= 1)
  moduleDomains . at domId ?= dom
  moduleGraph %= G.insNode (nodeId domId, ())
  -- add domain as subdomain of current root
  rootId <- use moduleRootDomain
  moduleDomains . ix rootId . domainSubdomains . contains domId .= True
  return domId

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
newDomain :: l -> Text -> Text -> Class l -> A.Annotation l -> Domain l
newDomain l name path cls ann = Domain
  { _domainName            = name
  , _domainPath            = path
  , _domainClassName       = cls ^. className . to A.getTypeName
  , _domainSubdomains      = S.empty
  , _domainPorts           = S.empty
  , _domainLabel           = l
  , _domainAnnotation      = ann
  , _domainClassAnnotation = cls ^. classAnnotation
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

-- | Get a domain by ID by looking in the current graph.
getDomain :: DomainId -> Eval l (Domain l)
getDomain domId = do
  domains <- use moduleDomains
  return $ domains ^?! ix domId

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

-- | Create graph edges between domains via ports, given a pair
-- of domains and ports and information about the connection.
makeEdges :: l
          -> (DomainId, PortId)
          -> (DomainId, PortId)
          -> ConnLevel
          -> A.ConnType
          -> A.Annotation l
          -> [G.LEdge (Connection l)]
makeEdges l (domL, portL) (domR, portR) level ty ann =
  [ (nodeId domL, nodeId domR, connF)
  , (nodeId domR, nodeId domL, connR)]
  where
    connF = Connection
              { _connectionLeft       = portL
              , _connectionRight      = portR
              , _connectionLevel      = level
              , _connectionType       = ty
              , _connectionLabel      = l
              , _connectionAnnotation = ann
              }
    connR = Connection
              { _connectionLeft       = portR
              , _connectionRight      = portL
              , _connectionLevel      = revLevel level
              , _connectionType       = A.revConnType ty
              , _connectionLabel      = l
              , _connectionAnnotation = ann
              }

-- | Return a list of graph edges for a connection between two
-- ports.
connectionEdges :: l
                -> A.PortName l
                -> A.PortName l
                -> A.ConnType
                -> A.Annotation l
                -> Eval l [G.LEdge (Connection l)]
connectionEdges l pidL pidR cty ann = do
  portL <- lookupPort pidL
  portR <- lookupPort pidR

  case (pidL, pidR) of
    (A.UPortName _, A.UPortName _) ->
      -- XXX m4-lobster currently outputs these, just ignore them for now
      return [] -- lose $ InternalConnection pidL pidR
    (A.QPortName _ _ _, A.QPortName _ _ _) ->
      return $ makeEdges l portL portR ConnLevelPeer cty ann
    (A.QPortName _ _ _, A.UPortName _) ->
      return $ makeEdges l portL portR ConnLevelParent cty ann
    (A.UPortName _, A.QPortName _ _ _) ->
      return $ makeEdges l portL portR ConnLevelChild cty ann

-- | Evaluate a statement and build up the graph.
evalStmt :: A.Annotation l -> A.Stmt l -> Eval l ()
evalStmt ann (A.StmtPortDecl l (A.VarName _ name) _) = do
  -- check for duplicate port definition
  whenM (isBound envPorts name) (lose $ DuplicatePort l name)
  -- create new port and add to graph
  path <- getMemberPath name
  domId <- use moduleRootDomain
  let port = Port name path Nothing Nothing l ann domId
  -- XXX ignoring port attributes for now
  portId <- addPort port
  moduleEnv . envPorts . at name ?= portId

evalStmt ann (A.StmtClassDecl l ty@(A.TypeName _ name) args body) = do
  whenM (isBound envClasses name) (lose $ DuplicateClass l name)
  path <- getClassPath name
  let cl = Class ty path args body ann
  addClass ty cl

evalStmt ann (A.StmtDomainDecl l (A.VarName _ name) ty args) = do
  whenM (isBound envSubdomains name) (lose $ DuplicateDomain l name)
  -- look up class and evaluate arguments
  cls    <- lookupClass ty
  env    <- subdomainEnv l cls args
  -- create new domain and add it to the graph
  domPath <- getMemberPath name
  let dom = newDomain l name domPath cls ann
  domId <- addSubdomain dom
  -- evaluate domain body in new environment
  subEnv <- inEnv domId env $ do
    evalStmts (cls ^. classBody)
    use moduleEnv
  -- add subdomain's environment to our environment
  moduleEnv . envSubdomains . at name ?= (domId, subEnv)

evalStmt _ (A.StmtAssign l (A.VarName _ name) e) = do
  whenM (isBound envVars name) (lose $ DuplicateVar l name)
  val <- evalExp e
  moduleEnv . envVars . at name ?= val

evalStmt ann (A.StmtConnection l pidL (A.ConnOp _ cty) pidR) = do
  -- TODO: This doesn't check for an existing connection.  Should
  -- we worry about that here or leave it for the type checker?
  edges <- connectionEdges l pidL pidR cty ann
  moduleGraph %= G.insEdges edges

evalStmt ann1 (A.StmtAnnotation _ ann2 stmt) = evalStmt (ann1 <> ann2) stmt

evalStmts :: [A.Stmt l] -> Eval l ()
evalStmts = mapM_ (evalStmt mempty)

-- | Evaluate a policy and return its graph or an error.
evalPolicy :: A.Policy l -> Either (Error l) (Module l)
evalPolicy (A.Policy l stmts) =
  execStateT (evalStmts stmts) (initialModule l)

----------------------------------------------------------------------
-- Graph Operations (move into new module)

{-
-- | Return the abstraction cost of an edge relation.
cost :: Relation -> Int
cost HasSubdomain = 1
cost (Connection ConnLevelChild _) = 1
cost _ = 0

-- | Return a subgraph following up to "n" subdomain edges.
--
-- This probably isn't very efficient as written.  We could
-- likely do better writing an algorithm by hand and following
-- subdomain edges directly from the root.
graphByLevel :: Int -> Graph -> Graph
graphByLevel n gr = G.delNodes nodes gr
  where
    costGraph = G.emap cost gr
    costTree  = G.spTree 0 costGraph
    pathFst (G.LP (x:_)) = x
    tooHigh (_, cost) = cost > n
    nodes = map fst $ filter tooHigh $ map pathFst costTree

-- | Load a Lobster file into a graph and write a Graphviz file
-- in "test.dot".  Used to test this module from GHCI.
test :: FilePath -> IO (Module Span)
test file = do
  A.Policy l xs <- P.test file
  let d = execStateT (evalStmts xs) (initialModule l)
  case d of
    Left err -> error (show err)
    Right x  -> return x
-}
