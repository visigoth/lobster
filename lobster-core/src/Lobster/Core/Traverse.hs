{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
--
-- Traverse.hs --- Lobster graph traversal.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

-- TODO: Consider renaming this module to "Analysis"?

-- TODO: Tighten up exports.
module Lobster.Core.Traverse where

import Control.Applicative
import Control.Error (hush)
import Control.Lens
import Control.Monad (foldM, forM_, unless)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer
import Data.List (find, foldl1')
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Monoid
import Data.Text (Text)
import Text.Parsec hiding (State, label, (<|>))
import Text.Parsec.Text ()

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

-- | Reverse a graph predicate.
revPred :: GConnPred -> GConnPred
revPred (PredPort a b) = SuccPort a b
revPred (SuccPort a b) = PredPort a b
revPred (EqPort a b)   = EqPort a b
revPred (And a b)      = And (revPred a) (revPred b)

-- | A graph connection.  This is "cooked" from the AST 'Connection'
-- type so that we don't have to repeatedly parse string-based
-- annotations and such.
data GConn = GConn
  { _gconnLeft        :: !PortId
  , _gconnRight       :: !PortId
  , _gconnPred        :: !(Maybe GConnPred)
  , _gconnId          :: !ConnectionId
  , _gconnLevel       :: !ConnLevel
  } deriving Show

instance Eq GConn where
  (==) a b =
    _gconnLeft  a == _gconnLeft  b &&
    _gconnRight a == _gconnRight b &&
    _gconnPred  a == _gconnPred  b &&
    _gconnLevel a == _gconnLevel b

instance Ord GConn where
  compare a b =
    compare (_gconnLeft  a) (_gconnLeft  b) <>
    compare (_gconnRight a) (_gconnRight b) <>
    compare (_gconnPred  a) (_gconnPred  b) <>
    compare (_gconnLevel a) (_gconnLevel b)

makeLenses ''GConn

-- | Return the conditional expression for a connection, if any.
gconnCond :: Module l -> GConn -> Maybe (Exp l)
gconnCond m gc =
  let conn = m ^. idConnection (gc ^. gconnId) in
  case lookupAnnotation "CondExpr" (conn ^. connectionAnnotation) of
    Just (e:[]) -> Just e
    _           -> Nothing

-- | Return the annotation for a connection.
gconnAnnotation :: Module l -> GConn -> Annotation l
gconnAnnotation m gc =
  m ^. idConnection (gc ^. gconnId) . connectionAnnotation

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
        , _gconnLevel  = conn ^. connectionLevel
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

-- | Reverse the direction of a connection.
revGConn :: GConn -> GConn
revGConn gc =
  GConn { _gconnLeft    = gc ^. gconnRight
        , _gconnRight   = gc ^. gconnLeft
        , _gconnPred    = revPred <$> gc ^. gconnPred
        , _gconnId      = gc ^. gconnId
        , _gconnLevel   = revLevel (gc ^. gconnLevel)
        }

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
-- Graph Traversal

-- | A permission to match against connections.
data Perm = PermAny !Text
          | PermClass !Text !Text
  deriving Show

instance Eq Perm where
  PermAny      p1 == PermAny      p2 = p1 == p2
  PermAny      p1 == PermClass _  p2 = p1 == p2
  PermClass _  p1 == PermAny      p2 = p1 == p2
  PermClass c1 p1 == PermClass c2 p2 = c1 == c2 && p1 == p2

instance Ord Perm where
  compare (PermClass c1 p1) (PermClass c2 p2) =
    compare c1 c2 <> compare p1 p2
  compare (PermClass _  p1) (PermAny      p2) =
    compare p1 p2
  compare (PermAny      p1) (PermClass _  p2) =
    compare p1 p2
  compare (PermAny      p1) (PermAny      p2) =
    compare p1 p2

-- | Parse a permission of the form "class.perm" or "*.perm".
parsePerm :: Text -> Maybe Perm
parsePerm t = hush (parse go "" t)
  where
    go        = permClass <|> permAny
    permAny   = PermAny   <$  char '*'
                          <*  char '.'
                          <*> fmap T.pack (many1 letter)
    permClass = PermClass <$> fmap T.pack (many1 letter)
                          <*  char '.'
                          <*> fmap T.pack (many1 letter)

-- | Return a set of permissions from a connection.
gconnPerms :: Module l -> GConn -> S.Set Perm
gconnPerms m gc = S.fromList (map go (lookupAnnotations "Perm" ann))
  where
    ann = m ^. idConnection (gc ^. gconnId) . connectionAnnotation
    go [ExpString (LitString _ cls), ExpString (LitString _ perm)] =
      PermClass cls perm
    go _ = error "malformed permission annotation"

-- | A path node in the traversal result.
data GTNode = GTNode
  { _gtnodeConn   :: GConn
  , _gtnodeNode   :: GNode
  , _gtnodeCond   :: Maybe (Exp ())
  } deriving (Show, Eq, Ord)

makeLenses ''GTNode

-- | A map of reachable domains and their paths.
newtype PathSet = PathSet { getPathSet :: M.Map DomainId (S.Set [GTNode]) }
  deriving Show

-- | Monoid instance that combines path lists.
instance Monoid PathSet where
  mempty = PathSet M.empty
  mappend (PathSet a) (PathSet b) =
    PathSet (M.unionWith S.union a b)

-- | Graph traversal edge function.  Given the current graph
-- context, return the list of outgoing edges that should be
-- followed during a graph traversal.
type EdgeF l = GContext l -> [(G.Node, GConn)]

-- | Edge function to traverse forward from subject to object.
forwardEdges :: EdgeF l
forwardEdges = G.lsuc'

-- | Edge function to traverse backward from objects to subjects.
--
-- XXX do we need to be reversing this here?
backwardEdges :: EdgeF l
backwardEdges ctx = [ (n, revGConn c)
                    | (n, c) <- G.lpre' ctx
                    ]


-- | Graph traversal environment.  This contains data that is
-- either invariant across the entire traversal, or locally
-- rebound at each level during traversal.
data GTEnv l = GTEnv
  { _gtenvModule      :: Module l             -- ^ lobster module
  , _gtenvEdgeF       :: EdgeF l              -- ^ edge function
  , _gtenvMaxDepth    :: Int                  -- ^ maximum depth
  , _gtenvLimit       :: Maybe Int            -- ^ leaf node limit
  , _gtenvIncoming    :: Maybe GConn          -- ^ incoming connection
  , _gtenvNextPred    :: Maybe GConnPred      -- ^ predicate on outgoing conn
  , _gtenvCond        :: Maybe (Exp l)        -- ^ conditional expr for this path
  , _gtenvPerms       :: Maybe (S.Set Perm)   -- ^ initial permission set
  , _gtenvTransPerms  :: S.Set Perm           -- ^ transitive permissions
  , _gtenvSegPerms    :: S.Set Perm           -- ^ current segment perms
  }

makeLenses ''GTEnv

-- | Create an initial environment given a set of parameters.
initialGTEnv :: Module l
             -> EdgeF l
             -> Maybe (S.Set Perm)
             -> S.Set Perm
             -> Int
             -> Maybe Int
             -> GTEnv l
initialGTEnv m f perms tperms maxD limit =
  GTEnv { _gtenvModule      = m
        , _gtenvEdgeF       = f
        , _gtenvMaxDepth    = maxD
        , _gtenvLimit       = limit
        , _gtenvIncoming    = Nothing
        , _gtenvNextPred    = Nothing
        , _gtenvCond        = Nothing
        , _gtenvPerms       = perms
        , _gtenvTransPerms  = tperms
        , _gtenvSegPerms    = S.empty
        }

-- | Graph traversal state.  This contains data that is modified
-- during the traversal independent of recursive depth.
data GTState = GTState
  { _gtstateResult  :: PathSet
  }

makeLenses ''GTState

-- | Initial graph traversal state.
initialGTState :: GTState
initialGTState = GTState mempty

-- | Combined reader/state monad used during graph traversal.
type GT l a = RWS.RWS (GTEnv l) () GTState a

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

withQueryLimit :: a -> GT l a -> GT l a
withQueryLimit z f = do
  lim  <- RWS.asks (view gtenvLimit)
  size <- RWS.gets (M.size . getPathSet . view gtstateResult)
  case lim of
    Just n | size < n  -> f
           | otherwise -> return z
    Nothing            -> f

getPaths1 :: Graph l -> G.Node -> [GTNode] -> Int -> Int -> GT l Bool
getPaths1 gr node path sn d = do
  env <- RWS.ask
  let maxD = env ^. gtenvMaxDepth
  if d > maxD
    then return True
    else getPaths2 gr node path sn d

getPaths2 :: Graph l -> G.Node -> [GTNode] -> Int -> Int -> GT l Bool
getPaths2 gr node path sn d =
  case G.match node gr of
    (Nothing, _)    -> return True
    (Just ctx, gr') -> do
      b <- processNode sn ctx path
      if b
        then getPaths3 gr' ctx path sn d
        else return True

isType :: GContext l -> GT l Bool
isType ctx = do
  m <- RWS.asks (view gtenvModule)
  case G.lab' ctx of
    GNodeDomain domId -> do
      let dom = m ^. idDomain domId
      let ann = dom ^. domainAnnotation
      return $ isJust $ lookupAnnotation "Type" ann
    GNodePort _ -> return False

subjectPort :: GConn -> GT l (Maybe (Port l))
subjectPort gc = do
  m <- RWS.asks (view gtenvModule)
  let portL = m ^. idPort (gc ^. gconnLeft)
  let portR = m ^. idPort (gc ^. gconnRight)
  if | portL ^. portPosition == PosSubject -> return $ Just portL
     | portR ^. portPosition == PosSubject -> return $ Just portR
     | otherwise                           -> return Nothing

isNewSegment :: GConn -> GT l Bool
isNewSegment gc = do
  m <- RWS.asks (view gtenvModule)
  let conn = m ^. idConnection (gc ^. gconnId)
  let ann  = conn ^. connectionAnnotation
  return $ isJust $ lookupAnnotation "Perm" ann

addResult :: GContext l -> [GTNode] -> GT l Bool
addResult ctx path = do
  let GNodeDomain domId = G.lab' ctx
  let ps = PathSet (M.singleton domId (S.singleton (reverse path)))
  gtstateResult <>= ps
  return True

filterPath :: Int -> GT l Bool
filterPath sn = do
  perms <- RWS.asks (view gtenvSegPerms)
  if sn == 1
    then do
      eperms <- RWS.asks (view gtenvPerms)
      case eperms of
        Just ps -> return $! not (S.null (S.intersection perms ps))
        Nothing -> return $! True
    else do
      etperms <- RWS.asks (view gtenvTransPerms)
      return $! not (S.null (S.intersection perms etperms))

processNode :: Int -> GContext l -> [GTNode] -> GT l Bool
processNode _  _   []   = return True
processNode sn ctx path = do
  b    <- filterPath sn
  isTy <- isType ctx
  if isTy
    then (do
      if b
        then addResult ctx path
        else return $! False)
    else return b

getPaths3 :: Graph l -> GContext l -> [GTNode] -> Int -> Int -> GT l Bool
getPaths3 gr ctx path sn d = do
  edgeF <- RWS.asks (view gtenvEdgeF)
  let edges = edgeF ctx
  foldM go True edges
  where
    go False _ = return False
    go True (n, l) =
      withConnPred l True $
        withConnState l $ do
          cond <- unlabelCond <$> RWS.asks (view gtenvCond)
          let path' = GTNode l (G.lab' ctx) cond : path
          withQueryLimit False $ do
            newSeg <- isNewSegment l
            if newSeg
              then do
                m        <- RWS.asks (view gtenvModule)
                let perms = gconnPerms m l
                RWS.local (gtenvSegPerms .~ perms) $
                  getPaths1 gr n path' (sn + 1) (d + 1)
              else do
                getPaths1 gr n path' sn (d + 1)

-- | Return the set of possible paths through a module's graph given
-- an edge function.
getPaths :: Module l
         -> EdgeF l
         -> Maybe (S.Set Perm)
         -> S.Set Perm
         -> Int
         -> Maybe Int
         -> Graph l
         -> G.Node
         -> (PathSet, Bool)
getPaths m f perms tperms maxD limit gr node = (r, a)
  where
    env       = initialGTEnv m f perms tperms maxD limit
    st        = initialGTState
    (a, s, _) = RWS.runRWS (getPaths1 gr node [] 0 0) env st
    r         = s ^. gtstateResult

----------------------------------------------------------------------
-- Simple SMT Generation

-- | A variable type for SMT generation.
data SMTType = SMTBool
  deriving (Eq, Ord, Show)

smtType :: SMTType -> Text
smtType SMTBool = "Bool"

-- | A variable name and its type.
data SMTVar = SMTVar Text SMTType
  deriving (Eq, Ord, Show)

-- | Return the free variables in an expression.
smtVars :: Exp l -> S.Set SMTVar
smtVars (ExpVar (VarName _ s))  = S.singleton $ SMTVar s SMTBool
smtVars (ExpBinaryOp _ e1 _ e2) = S.union (smtVars e1) (smtVars e2)
smtVars (ExpUnaryOp _ _ e)      = smtVars e
smtVars (ExpParen _ e)          = smtVars e
smtVars _                       = S.empty

smtBinOp :: Text -> Exp l -> Exp l -> Text
smtBinOp t e1 e2 = "(" <> t <> " " <> smtExp e1 <> " " <> smtExp e2 <> ")"

-- | Return a declaration for an SMT variable.
smtVarDecl :: SMTVar -> Text
smtVarDecl (SMTVar s ty) =
  "(declare-fun " <> s <> " () " <> smtType ty <> ")"

-- | Return an SMT expression for a Lobster expression.
smtExp :: Exp l -> Text
smtExp (ExpVar (VarName _ s)) = s
smtExp (ExpBinaryOp _ e1 BinaryOpAnd e2) = smtBinOp "and" e1 e2
smtExp (ExpBinaryOp _ e1 BinaryOpOr  e2) = smtBinOp "or"  e1 e2
smtExp (ExpBinaryOp _ e1 BinaryOpEqual e2) = smtBinOp "=" e1 e2
smtExp (ExpBinaryOp l e1 BinaryOpNotEqual e2) =
  smtExp (ExpUnaryOp l UnaryOpNot (ExpBinaryOp l e1 BinaryOpEqual e2))
smtExp (ExpParen _ e) = smtExp e
smtExp _ = error "invalid expression for SMT"

-- | Return an SMT assertion for a Lobster condition.
smtAssert :: Exp l -> Text
smtAssert e = "(assert " <> smtExp e <> ")"

smt :: Exp l -> Text
smt e = T.intercalate "\n" decls
  where
    vars    = map smtVarDecl (S.toList (smtVars e))
    asserts = [smtAssert e]
    decls   = vars ++ asserts

