{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- Traverse.hs --- Lobster graph traversal.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Lobster.Core.Traverse where

import Control.Lens
import Control.Monad.Trans.Writer
import Data.Maybe (isNothing)
import Data.Monoid
import Data.Text (Text)

import Lobster.Core.Eval

import qualified Data.Graph.Inductive as G
import qualified Data.Set             as S
import qualified Data.Text            as T

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
connectionsWith m p = filter go (map (view _3) (G.labEdges (m ^. moduleGraph)))
  where
    getDomain x     = m ^. idDomain x
    getPort   x     = m ^. idPort x
    getPortDomain x = getDomain $ view portDomain (getPort x)
    go conn = getDomainPred p (getPortDomain (conn ^. connectionLeft)) ||
              getDomainPred p (getPortDomain (conn ^. connectionRight))

----------------------------------------------------------------------
-- Domain Subgraphs

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
