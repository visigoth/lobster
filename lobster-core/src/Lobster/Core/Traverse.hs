{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- Traverse.hs --- Lobster graph traversal.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

-- TODO: Consider renaming this module to "Analysis"?

module Lobster.Core.Traverse where

import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Monoid
import Data.Text (Text)

import Lobster.Core.AST
import Lobster.Core.Eval

import qualified Data.Graph.Inductive as G
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Text            as T

----------------------------------------------------------------------
-- Graph Building

type Graph l = G.Gr () (Connection l)

-- | Return true if an edge already exists in the graph.
edgeExists :: Graph l -> (Int, Int, Connection l) -> Bool
edgeExists gr (nodeL, nodeR, _) = elem nodeR (G.suc gr nodeL)   -- argh, O(n)

addEdges :: Graph l -> [(Int, Int, Connection l)] -> Graph l
addEdges gr edges = G.insEdges edges gr

-- | Reverse a connection level if a parent/child type.
revLevel :: ConnLevel -> ConnLevel
revLevel ConnLevelParent   = ConnLevelChild
revLevel ConnLevelChild    = ConnLevelParent
revLevel ConnLevelPeer     = ConnLevelPeer
revLevel ConnLevelInternal = ConnLevelInternal

-- | Reverse annotations for a connection.  This switches "Lhs"
-- to "Rhs" and vice versa.
--
-- XXX i'm not wild about this, but it seems necessary
revAnnotation :: Annotation l -> Annotation l
revAnnotation (Annotation xs) = Annotation (map go xs)
  where
    go (ty@(TypeName l name), args)
      | name == "Lhs" = (TypeName l "Rhs", args)
      | name == "Rhs" = (TypeName l "Lhs", args)
      | otherwise     = (ty,                 args)

revConn :: Connection l -> Connection l
revConn conn = (`execState` conn) $ do
  connectionLevel      %= revLevel
  connectionType       %= revConnType
  connectionAnnotation %= revAnnotation

-- | Direction of the graph edge to create for a connection
-- based on port position.
data EdgeDirection = LeftToRight
                   | RightToLeft
                   | Bidirectional
  deriving (Eq, Ord, Show)

connLeftPos :: Module l -> Connection l -> Position
connLeftPos m conn =
  case level of
    ConnLevelParent -> revPosition pos
    _               -> pos
  where
    level = conn ^. connectionLevel
    port  = m ^. idPort (conn ^. connectionLeft)
    pos   = port ^. portPosition

connRightPos :: Module l -> Connection l -> Position
connRightPos m conn =
  case level of
    ConnLevelChild -> revPosition pos
    _              -> pos
  where
    level = conn ^. connectionLevel
    port  = m ^. idPort (conn ^. connectionRight)
    pos   = port ^. portPosition

{-
edgeDirection :: Connection l -> EdgeDirection
edgeDirection conn =
  case (
-}

-- | Add a single connection between two ports.  If the ports are
-- typed with a subject/object relationship, only a single edge
-- will be created.
addConnection :: Module l -> Graph l -> Connection l -> Graph l
addConnection m gr conn = addEdges gr edges
  where
    portL = m ^. idPort (conn ^. connectionLeft)
    portR = m ^. idPort (conn ^. connectionRight)
    posL  = connLeftPos m conn
    posR  = connRightPos m conn
    domL  = portL ^. portDomain
    domR  = portR ^. portDomain
    fEdge = (nodeId domL, nodeId domR, conn)
    bEdge = (nodeId domL, nodeId domR, revConn conn)
    edges = case (posL, posR) of
              (PosSubject, PosObject)  -> [fEdge]
              (PosObject,  PosSubject) -> [bEdge]
              _                        -> [fEdge, bEdge]
                

-- | Build a graph of connections between domains from a Lobster module.
moduleGraph :: Module l -> Graph l
moduleGraph m = M.foldl' (addConnection m) gr (m ^. moduleConnections)
  where
    domains = m ^. moduleDomains
    gr = G.insNodes [(k, ()) | DomainId k <- M.keys domains] G.empty

-- | Return a labelled graph for use with graphviz.
labelledModuleGraph :: Module l -> G.Gr Text Text
labelledModuleGraph m = G.emap goE (G.gmap goN (moduleGraph m))
  where
    getName :: PortId -> Text
    getName p = m ^?! modulePorts . ix p . portName
    {-
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
{-
connectionsWith m p = filter go (map (view _3) (G.labEdges (m ^. moduleGraph)))
  where
    getDomain x     = m ^. idDomain x
    getPort   x     = m ^. idPort x
    getPortDomain x = getDomain $ view portDomain (getPort x)
    go conn = getDomainPred p (getPortDomain (conn ^. connectionLeft)) ||
              getDomainPred p (getPortDomain (conn ^. connectionRight))
-}

----------------------------------------------------------------------
-- Domain Subgraphs

{-
-- XXX this is no longer used

-- | Predicate that returns true for the root domain.
isRootDomain :: DomainPred l
isRootDomain = DomainPred (isNothing . view domainParent)

-- | Return a module's domain graph with domains that do not
-- match the predicate removed.
subgraphWith :: Module l -> DomainPred l -> Graph l
subgraphWith m p = G.efilter go gr
  where
    gr = m ^. moduleGraph
    -- always retain root domain for consistency with domtrees
    p' = isRootDomain <> p
    go (n1, n2, _) =
      getDomainPred p' (m ^. idDomain (DomainId n1)) &&
      getDomainPred p' (m ^. idDomain (DomainId n2))
-}

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
