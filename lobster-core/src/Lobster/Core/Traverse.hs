{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--
-- Traverse.hs --- Lobster graph traversal.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

-- TODO: Consider renaming this module to "Analysis"?

-- TODO: Tighten up exports.
module Lobster.Core.Traverse where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (foldM, forM_, unless)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer
import Data.List (find, foldl1')
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Monoid
import Data.Text (Text)
import Data.Tree

import Lobster.Core.AST
import Lobster.Core.Eval

import qualified Control.Monad.Trans.RWS.Strict    as RWS
import qualified Data.Foldable                     as F
import qualified Data.Graph.Inductive              as G
import qualified Data.Graph.Inductive.PatriciaTree as GP
import qualified Data.Map.Strict                   as M
import qualified Data.Set                          as S
import qualified Data.Text                         as T

----------------------------------------------------------------------
-- Graph Building
--
-- The nodes in the graph are either domains or ports.
--
-- For explicit domains, we create a node for each port, and create
-- an edge between ports for each connection.  This edge is directed
-- if there is a subject/object relationship between the ports.
--
-- For implicit (normal) domains, we can merge all the ports into
-- a single node.

-- | Node label indicating whether the node is a domain or port.
data GNode = GNodeDomain !DomainId
           | GNodePort   !PortId
  deriving (Eq, Ord, Show)

makePrisms ''GNode

-- | A graph connection predicate.  This is created from annotations
-- on the connection as defined in the Lobster source.
--
-- FIXME: I would rather use port IDs rather than names here, but
-- the importer doesn't generate properly qualified names, so they
-- are actually ambiguous...  We spend a lot of time doing text
-- comparisons on the visited set.
data GConnPred
  = PredPort !Text !Text
  | SuccPort !Text !Text
  | EqPort !Text !Text
  | And GConnPred GConnPred
  deriving (Eq, Ord, Show)

makePrisms ''GConnPred

-- | A graph connection.  This is "cooked" from the AST 'Connection'
-- type so that we don't have to repeatedly parse string-based
-- annotations and such.
data GConn = GConn
  { _gconnLeft        :: !PortId
  , _gconnRight       :: !PortId
  , _gconnPred        :: !(Maybe GConnPred)
  , _gconnId          :: !ConnectionId
  } deriving Show

instance Eq GConn where
  (==) a b =
    _gconnLeft  a == _gconnLeft  b &&
    _gconnRight a == _gconnRight b &&
    _gconnPred  a == _gconnPred  b

instance Ord GConn where
  compare a b =
    compare (_gconnLeft  a) (_gconnLeft  b) <>
    compare (_gconnRight a) (_gconnRight b) <>
    compare (_gconnPred  a) (_gconnPred  b)

makeLenses ''GConn

-- | Return the conditional expression for a connection, if any.
gconnCond :: Module l -> GConn -> Maybe (Exp l)
gconnCond m gc =
  let conn = m ^. idConnection (gc ^. gconnId) in
  case lookupAnnotation "CondExpr" (conn ^. connectionAnnotation) of
    Just (e:[]) -> Just e
    _           -> Nothing

-- | A graph of domains and ports with edges labelled with
-- connection information.
type Graph l = GP.Gr GNode GConn

-- | A context for our graph type.
type GContext l = G.Context GNode GConn

-- | State of the graph builder.  We maintain a reverse mapping
-- of domain and port IDs to their respective nodes.  When we
-- create a connection, we first look to see if each port has
-- a specific node, and use that if it exists.  If not, we connect
-- to the port's owning domain.
data GState l = GState
  { _gstateModule       :: !(Module l)
  , _gstateGraph        :: !(Graph l)
  , _gstateNextNodeId   :: !Int
  , _gstateDomainMap    :: !(M.Map DomainId Int)
  , _gstatePortMap      :: !(M.Map PortId   Int)
  }

makeLenses ''GState

data ModuleGraph l = ModuleGraph
  { _moduleGraphGraph     :: !(Graph l)
  , _moduleGraphDomainMap :: !(M.Map DomainId Int)
  , _moduleGraphPortMap   :: !(M.Map PortId   Int)
  }

makeLenses ''ModuleGraph

-- | State monad used internally during graph building.
type G l a = State (GState l) a

-- | Create a predicate for a single annotation element.
annEltPred :: AnnotationElement l -> Maybe GConnPred
annEltPred ae =
  case ae of
    (TypeName _ ty, ExpVar (VarName _ v1):ExpVar (VarName _ v2):[])
      | ty == "Lhs" -> Just $ PredPort v1 v2
      | ty == "Rhs" -> Just $ SuccPort v1 v2
    _ -> Nothing

-- | Create a predicate for a connection annotation.
annPred :: Annotation l -> Maybe GConnPred
annPred (Annotation xs) =
  case mapMaybe annEltPred xs of
    [] -> Nothing
    ys -> Just $ foldl1' And ys

-- | Create a graph connection from an AST connection.
gconn :: Connection l -> GConn
gconn conn =
  GConn { _gconnLeft   = conn ^. connectionLeft
        , _gconnRight  = conn ^. connectionRight
        , _gconnPred   = annPred $ conn ^. connectionAnnotation
        , _gconnId     = conn ^. connectionId
        }

-- | Add a domain to the current graph.
addDomain :: Domain l -> G l ()
addDomain d
  | d ^. domainIsExplicit = addExplicitDomain d
  | otherwise             = addImplicitDomain d

-- | Add an explicit domain to the current graph.
addExplicitDomain :: Domain l -> G l ()
addExplicitDomain d = F.mapM_ addPort (d ^. domainPorts)
  where
    addPort pid = do
      nid <- gstateNextNodeId <<+= 1
      gstateGraph %= G.insNode (nid, GNodePort pid)
      gstatePortMap . at pid ?= nid

-- | Add an implicit domain to the current graph.
addImplicitDomain :: Domain l -> G l ()
addImplicitDomain d = do
  let domId = d ^. domainId
  nid <- gstateNextNodeId <<+= 1
  gstateGraph %= G.insNode (nid, GNodeDomain domId)
  gstateDomainMap . at domId ?= nid

-- | Reverse a connection level if a parent/child type.
revLevel :: ConnLevel -> ConnLevel
revLevel ConnLevelParent   = ConnLevelChild
revLevel ConnLevelChild    = ConnLevelParent
revLevel ConnLevelPeer     = ConnLevelPeer
revLevel ConnLevelInternal = ConnLevelInternal

-- | Reverse annotations for a connection.  This switches "Lhs"
-- to "Rhs" and vice versa.
revAnnotation :: Annotation l -> Annotation l
revAnnotation (Annotation xs) = Annotation (map go xs)
  where
    go (ty@(TypeName l name), args)
      | name == "Lhs" = (TypeName l "Rhs", args)
      | name == "Rhs" = (TypeName l "Lhs", args)
      | otherwise     = (ty,                 args)

revConn :: Connection l -> Connection l
revConn conn = (`execState` conn) $ do
  portL                <- use connectionLeft
  portR                <- use connectionRight
  connectionLevel      %= revLevel
  connectionType       %= revConnType
  connectionAnnotation %= revAnnotation
  connectionLeft       .= portR
  connectionRight      .= portL

connLeftPos :: Module l -> Connection l -> Position
connLeftPos m conn =
  case level of
    ConnLevelParent   -> revPosition pos
    ConnLevelInternal -> revPosition pos
    _                 -> pos
  where
    level = conn ^. connectionLevel
    port  = m ^. idPort (conn ^. connectionLeft)
    pos   = port ^. portPosition

connRightPos :: Module l -> Connection l -> Position
connRightPos m conn =
  case level of
    ConnLevelChild    -> revPosition pos
    ConnLevelInternal -> revPosition pos
    _                 -> pos
  where
    level = conn ^. connectionLevel
    port  = m ^. idPort (conn ^. connectionRight)
    pos   = port ^. portPosition

-- | Get the node ID for a connection to a port.
getNodeId :: Port l -> Domain l -> G l Int
getNodeId port dom = do
  portMap <- use gstatePortMap
  domMap  <- use gstateDomainMap
  return $! fromJust $
    (portMap ^? ix (port ^. portId))   <|>
    (domMap  ^? ix (dom  ^. domainId)) <|>
    (error "internal error: node not found")    -- shouldn't happen

-- | Return true if an edge already exists between nodes
-- with the same connection.
edgeExists :: (Int, Int, GConn) -> G l Bool
edgeExists (nodeL, nodeR, conn) = do
  gr <- use gstateGraph
  let sucs = G.lsuc gr nodeL
  return $! isJust $! find (\(n, c) -> n == nodeR && c == conn) sucs

-- | Add a single connection between two ports.  If the ports are
-- typed with a subject/object relationship, only a single edge
-- will be created.
addConnection :: Connection l -> G l ()
addConnection conn = do
  m <- use gstateModule
  let portL = m ^. idPort (conn ^. connectionLeft)
  let portR = m ^. idPort (conn ^. connectionRight)
  let posL  = connLeftPos m conn
  let posR  = connRightPos m conn
  let domL  = m ^. idDomain (portL ^. portDomain)
  let domR  = m ^. idDomain (portR ^. portDomain)
  nodeL    <- getNodeId portL domL
  nodeR    <- getNodeId portR domR
  let fEdge = (nodeL, nodeR, gconn $ conn)
  let bEdge = (nodeR, nodeL, gconn $ revConn conn)
  let edges = case (posL, posR) of
                (PosSubject, PosObject)  -> [fEdge]
                (PosObject,  PosSubject) -> [bEdge]
                _                        -> [fEdge, bEdge]
  forM_ edges $ \edge -> do
    exists <- edgeExists edge
    unless exists $
      gstateGraph %= G.insEdge edge

-- | Build a graph of connections between domains from a Lobster module.
moduleGraph :: Eq l => Module l -> ModuleGraph l
moduleGraph m = evalState go st
  where
    go = do
      mapMOf_ (moduleDomains     . folded) addDomain m
      mapMOf_ (moduleConnections . folded) addConnection m
      gr      <- use gstateGraph
      domMap  <- use gstateDomainMap
      portMap <- use gstatePortMap
      return $! ModuleGraph gr domMap portMap
    st = GState m G.empty 0 M.empty M.empty

-- | Return a labelled graph for use with graphviz.
labelledModuleGraph :: Eq l => Module l -> GP.Gr Text Text
labelledModuleGraph m = G.emap goE (G.gmap goN gr)
  where
    gr = moduleGraph m ^. moduleGraphGraph
    goE = const ""
    goN (preds, node, GNodeDomain domId, posts) =
      let name = m ^?! moduleDomains . ix domId . domainPath in
      (preds, node, name, posts)
    goN (preds, node, GNodePort pid, posts) =
      let name = m ^?! modulePorts . ix pid . portPath in
      (preds, node, name, posts)

----------------------------------------------------------------------
-- Domain Trees

-- | A predicate on the domains in a Lobster graph.
newtype DomainPred l = DomainPred { getDomainPred :: Domain l -> Bool }

instance Monoid (DomainPred l) where
  mempty = DomainPred (const False)
  mappend (DomainPred x) (DomainPred y) = DomainPred $ \z -> x z || y z

noDomainPred :: DomainPred l
noDomainPred = DomainPred (const True)

-- | A view of the subdomain tree without indirection through IDs.
data DomainTree l = DomainTree
  { _domainTreeDomain      :: Domain l
  , _domainTreeSubdomains  :: [DomainTree l]
  , _domainTreePorts       :: [Port l]
  } deriving (Show, Functor)

makeLenses ''DomainTree

-- | Build a domain tree with a predicate indicating which subdomains
-- to recurse into.  This will always contain, at minimum, the root
-- domain.
domainTreeWith :: Module l -> DomainPred l -> DomainTree l
domainTreeWith m p = makeDomainTree m p rootDom
  where rootDom = m ^. idDomain (m ^. moduleRootDomain)

-- | Build a complete domain tree from a module.
domainTree :: Module l -> DomainTree l
domainTree m = domainTreeWith m noDomainPred

-- | Build one level of a domain tree.
makeDomainTree :: Module l -> DomainPred l -> Domain l -> DomainTree l
makeDomainTree m p dom = DomainTree
  { _domainTreeDomain      = dom
  , _domainTreeSubdomains  = subdomainTrees
  , _domainTreePorts       = ports
  }
  where
    getDomain x    = m ^. idDomain x
    subdomainIds   = S.toList (dom ^. domainSubdomains)
    subdomains     = filter (getDomainPred p) (map getDomain subdomainIds)
    subdomainTrees = map (makeDomainTree m p) subdomains
    getPort x      = m ^. idPort x
    ports          = map getPort $ S.toList (dom ^. domainPorts)

-- | Fold a function over all nodes in a domain tree.
foldDomainTree :: Monoid m => (DomainTree l -> m) -> DomainTree l -> m
foldDomainTree f dt = f dt <> rest
  where
    rest = mconcat (map (foldDomainTree f) (dt ^. domainTreeSubdomains))

-- | Return a flattened list of all domain tree nodes.
flattenDomainTree :: DomainTree l -> [DomainTree l]
flattenDomainTree = foldDomainTree (:[])

-- | Return a flat list of all domains in a domain tree.
allDomains :: DomainTree l -> [Domain l]
allDomains = foldDomainTree ((:[]) . (view domainTreeDomain))

-- | Return a flat list of all ports in a domain tree.
allPorts :: DomainTree l -> [Port l]
allPorts = foldDomainTree (view domainTreePorts)

-- | Return a list of all connections in which at least one of the
-- domains satifies the given predicate.
connectionsWith :: Module l -> DomainPred l -> [Connection l]
connectionsWith m p = M.elems $ M.filter go (m ^. moduleConnections)
  where
    getDomain x     = m ^. idDomain x
    getPort   x     = m ^. idPort x
    getPortDomain x = getDomain $ view portDomain (getPort x)
    go conn = getDomainPred p (getPortDomain (conn ^. connectionLeft)) ||
              getDomainPred p (getPortDomain (conn ^. connectionRight))

----------------------------------------------------------------------
-- Domain Predicates

-- | Return the depth from the root of a domain.
depth :: Module l -> Domain l -> Int
depth m dom =
  maybe 0 (\x -> 1 + depth m (m ^. idDomain x)) (dom ^. domainParent)

-- | Predicate on the maximum depth of a domain.
maxDepth :: Module l -> Int -> Domain l -> Bool
maxDepth m x dom = depth m dom <= x

-- | Predicate to match a domain by ID.
isDomainId :: DomainId -> Domain l -> Bool
isDomainId domId dom = domId == dom ^. domainId

-- | Match a domain path against a path string.
--
-- The second path may end with a wildcard "*" which will match
-- any subdomain.
matchPath :: Text -> Text -> Bool
matchPath domPath path = go (splitPath domPath) (splitPath path)
  where
    splitPath = T.split (== '.')
    go [] _ = True
    go (p1:p1s) x@(p2:p2s)
      | p2 == "*" && null p2s = go p1s x
      | p1 == p2 = go p1s p2s
      | otherwise = False
    go _ _ = False

-- | Predicate on the path of a domain.  This matches all domains
-- along the path from the root.
isDomainPath :: Text -> Domain l -> Bool
isDomainPath path dom = matchPath (dom ^. domainPath) path

-- | Helper writer monad for building domain predicates.
type DomainPredBuilder l a = Writer (DomainPred l) a

-- | Add a predicate to the one currently being built up.
addPred :: (Domain l -> Bool) -> DomainPredBuilder l ()
addPred = tell . DomainPred

-- | Run a domain predicate builder and return the combined predicate.
runDomainPredBuilder :: DomainPredBuilder l a -> DomainPred l
runDomainPredBuilder = execWriter

----------------------------------------------------------------------
-- Graph Traversal Types

-- | Graph traversal edge function.  Given the current graph
-- context, return the list of outgoing edges that should be
-- followed during a graph traversal.
type EdgeF l = GContext l -> [(G.Node, GConn)]

-- | Edge function to traverse forward from subject to object.
forwardEdges :: EdgeF l
forwardEdges = G.lsuc'

-- | Edge function to traverse backward from objects to subjects.
backwardEdges :: EdgeF l
backwardEdges = G.lpre'

-- | Graph traversal environment.  This contains data that is
-- either invariant across the entire traversal, or locally
-- rebound at each level during traversal.
data GTEnv l = GTEnv
  { _gtenvModule   :: Module l           -- ^ lobster module
  , _gtenvEdgeF    :: EdgeF l            -- ^ edge function
  , _gtenvMaxDepth :: Int                -- ^ maximum depth
  , _gtenvLimit    :: Maybe Int          -- ^ leaf node limit
  , _gtenvIncoming :: Maybe GConn        -- ^ incoming connection
  , _gtenvNextPred :: Maybe GConnPred    -- ^ predicate on outgoing conn
  , _gtenvCond     :: Maybe (Exp l)      -- ^ conditional expr for this path
  }

makeLenses ''GTEnv

-- | Create an initial environment given a set of parameters.
initialGTEnv :: Module l -> EdgeF l -> Int -> Maybe Int -> GTEnv l
initialGTEnv m f maxD limit =
  GTEnv { _gtenvModule    = m
        , _gtenvEdgeF     = f
        , _gtenvMaxDepth  = maxD
        , _gtenvLimit     = limit
        , _gtenvIncoming  = Nothing
        , _gtenvNextPred  = Nothing
        , _gtenvCond      = Nothing
        }

-- | Graph traversal state.  This contains data that is modified
-- during the traversal independent of recursive depth.
data GTState l = GTState
  deriving (Show, Eq, Ord)

makeLenses ''GTState

-- | Initial graph traversal state.
initialGTState :: GTState l
initialGTState = GTState

-- | A path node in a traversal result.  Contains the connection
-- that was followed, along with the conditional expression that
-- must be satisfied.
data PathNode = PathNode
  { _pathNodeConn   :: !GConn
  , _pathNodeExp    :: !(Maybe (Exp ()))
  } deriving (Show, Eq, Ord)

makeLenses ''PathNode

-- | A graph traversal result---either a full or partial tree
-- of paths originating from the start node.
data GTResult = GTFull    [Tree PathNode]
              | GTPartial [Tree PathNode]

-- | Map a function over the forest in a traversal result.
--
-- (this is just monomorphic fmap)
mapGTResult :: ([Tree PathNode] -> [Tree PathNode]) -> GTResult -> GTResult
mapGTResult f (GTFull xs) = GTFull (f xs)
mapGTResult f (GTPartial xs) = GTPartial (f xs)

-- | Monoid instance for a traversal result.
instance Monoid GTResult where
  mempty = GTFull []
  mappend (GTFull a)    (GTFull    b) = GTFull    (a ++ b)
  mappend (GTFull a)    (GTPartial b) = GTPartial (a ++ b)
  mappend (GTPartial a) (GTFull    b) = GTPartial (a ++ b)
  mappend (GTPartial a) (GTPartial b) = GTPartial (a ++ b)

-- | Combined reader/state monad used during graph traversal.
type GT l a = RWS.RWS (GTEnv l) () (GTState l) a

----------------------------------------------------------------------
-- Graph Traversal Predicates

-- | Combine two connection predicates.
unionPred :: Maybe GConnPred -> Maybe GConnPred -> Maybe GConnPred
unionPred Nothing  Nothing  = Nothing
unionPred (Just x) Nothing  = Just x
unionPred Nothing  (Just y) = Just y
unionPred (Just x) (Just y) = Just (And x y)

-- | Combine two conditional expressions.
unionCond :: Maybe (Exp l) -> Maybe (Exp l) -> Maybe (Exp l)
unionCond Nothing   Nothing   = Nothing
unionCond (Just e1) Nothing   = Just e1
unionCond Nothing   (Just e2) = Just e2
unionCond (Just e1) (Just e2) =
  -- XXX using source position of first expression only
  Just (ExpBinaryOp (label e1) e1 BinaryOpAnd e2)

-- | Unlabel a conditional expression.
unlabelCond :: Maybe (Exp l) -> Maybe (Exp ())
unlabelCond = fmap (fmap (const ()))

-- | Return true if a port matches a predicate's name.
checkPort :: Module l -> PortId -> Text -> Text -> Bool
checkPort m pid dName pName =
  let port = m ^. idPort pid in
  let dom  = m ^. idDomain (port ^. portDomain) in
  dom ^. domainName == dName && port ^. portName == pName

-- | Return the domain of the right-hand port of a connection.
rightDomain :: Module l -> GConn -> DomainId
rightDomain m conn =
  let port = m ^. idPort (conn ^. gconnRight) in
  port ^. portDomain

-- | Evaluate a predicate against a connection and environment.
--
-- Returns a boolean and any additional predicates to union
-- into the next hop.
evalPred :: GTEnv l -> GConn -> GConnPred -> (Bool, Maybe GConnPred)
evalPred env conn (And x y) =
  let (b1, p1) = evalPred env conn x in
  let (b2, p2) = evalPred env conn y in
  if b1 && b2
    then (True, unionPred p1 p2)
    else (False, Nothing)
-- equal port: check right port of current connection
evalPred env conn (EqPort t1 t2) =
  (checkPort (env ^. gtenvModule) (conn ^. gconnRight) t1 t2, Nothing)
-- predeccessor port: check left port of incoming connection
evalPred env _ (PredPort t1 t2) =
  case env ^. gtenvIncoming of
    Just x   -> (checkPort (env ^. gtenvModule) (x ^. gconnLeft) t1 t2, Nothing)
    Nothing  -> (False, Nothing)
-- successor port: add an 'EqPort' to the next hop
evalPred _ _ (SuccPort t1 t2) =
  (True, Just (EqPort t1 t2))

-- | Return true if a connection is non-negative during traversal.
isntNegative :: GConn -> GT l Bool
isntNegative l = do
  env <- RWS.ask
  let m = env ^. gtenvModule
  case env ^. gtenvIncoming of
    Just inc -> return $! isntNegativeConn m (inc ^. gconnRight) (l ^. gconnLeft)
    Nothing  -> return $! True
 
-- | Evaluate a connection's predicate against the current
-- traversal environment, executing the body in a locally
-- modified environment if the connection should be followed,
-- or returning a default value if not.  This also checks for
-- negative connections.
withConnPred :: GConn -> a -> GT l a -> GT l a
withConnPred conn z f = do
  -- union the current predicate with the connection's
  env       <- RWS.ask
  let pred1  = env ^. gtenvNextPred
  let pred2  = conn ^. gconnPred
  let pred3  = unionPred pred1 pred2
  case pred3 of
    Just x -> do
      let (b, pred4) = evalPred env conn x
      notNeg <- isntNegative conn
      -- predicate exists, if it is true, execute body
      -- with locally bound next hop predicate
      if b && notNeg
        then RWS.local (gtenvNextPred .~ pred4) f
        else return z
    -- no predicate, execute body in unmodified env
    -- if connection is non-negative
    Nothing -> do
      notNeg <- isntNegative conn
      if notNeg
        then f
        else return z

-- | Locally bind the environment's incoming connection and
-- conditional expression for a connection that is about to
-- be traversed.
withConnState :: GConn -> GT l a -> GT l a
withConnState l f = RWS.local go f
  where
    go env =
      let cond = gconnCond (env ^. gtenvModule) l in
        env & gtenvIncoming .~ Just l
            & gtenvCond     %~ unionCond cond

-- | Return a forest of possible paths through a module's graph given
-- an edge function.
getPaths :: Module l -> EdgeF l -> Int -> Graph l -> G.Node -> [Tree PathNode]
getPaths m f maxD gr node =
  case fst $ RWS.evalRWS (getPaths1 gr node 0) env st of
    GTFull ts    -> ts
    GTPartial ts -> ts
  where
    env = initialGTEnv m f maxD Nothing
    st  = initialGTState

getPaths1 :: Graph l -> G.Node -> Int -> GT l GTResult
getPaths1 gr node d = do
  env <- RWS.ask
  let maxD = env ^. gtenvMaxDepth
  if d > maxD
    then return mempty
    else getPaths2 gr node d

getPaths2 :: Graph l -> G.Node -> Int -> GT l GTResult
getPaths2 gr node d =
  case G.match node gr of
    (Nothing, _)    -> return mempty
    (Just ctx, gr') -> do
      env       <- RWS.ask
      let inc    = env ^. gtenvIncoming
      let edgeF  = env ^. gtenvEdgeF
      let edges  = edgeF ctx
      forest    <- foldM (go gr') mempty edges
      case inc of
        Just gc ->
          let cond = unlabelCond $ env ^. gtenvCond in
            return $ mapGTResult (return . Node (PathNode gc cond)) forest
        Nothing -> return forest
  where
    go g ts1 (n, l) =
      withConnPred l ts1 $
        withConnState l $ do
          ts2 <- getPaths1 g n (d + 1)
          return (ts1 <> ts2)

----------------------------------------------------------------------
-- Path Sets

-- | Result of a path query---a mapping of leaf domains to the paths
-- from the initial node.
type PathSet = M.Map DomainId (S.Set [PathNode])

-- | Create a path set from a single path tree.
makePathSet :: forall l. Module l -> Tree PathNode -> PathSet
makePathSet m t = go M.empty [] t
  where
    go :: PathSet -> [PathNode] -> Tree PathNode -> PathSet
    go ps path (Node x []) =
      ps & at (rightDomain m (x ^. pathNodeConn)) . non S.empty %~ S.insert (reverse (x : path))
    go ps path (Node x xs) =
      M.unionsWith S.union (map (go ps (x : path)) xs)

-- | Create a path set from a forest of path trees.
getPathSet :: Module l -> [Tree PathNode] -> PathSet
getPathSet m ts = M.unionsWith S.union (map (makePathSet m) ts)


