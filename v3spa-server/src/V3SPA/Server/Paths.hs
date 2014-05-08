{-# LANGUAGE OverloadedStrings #-}
--
-- Paths.hs --- Path queries on a Lobster module.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module V3SPA.Server.Paths
  ( handlePaths
  ) where

import Control.Error
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Text (pack)
import Snap

import Lobster.Core
import V3SPA.Server.Snap

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

-- | Get the domain ID from query parameters.
paramDomainId :: V3Snap (Maybe DomainId)
paramDomainId = do
  r <- getQueryParam "id"
  return $ maybe Nothing ((DomainId . fst <$>) . BS.readInt) r

-- | Output a single path as JSON.
pathJSON :: [GConn] -> Value
pathJSON xs = toJSON (map (getConnectionId . view gconnId) xs)

-- | Output a path set as JSON.
pathSetJSON :: PathSet -> Value
pathSetJSON ps =
  object [ (pack $ show domId) .= (map pathJSON (S.toList s))
         | (DomainId domId, s) <- M.toList ps
         ]

-- | "POST /paths?id=N" --- path query
handlePaths :: V3Snap ()
handlePaths = method POST $ do
  modifyResponse $ setContentType "application/json"
  body   <- readRequestBody 10000000
  m      <- hoistErr $ readPolicyBS body

  domId  <- hoistMiscErr =<< (note "parameter 'id' not supplied" <$> paramDomainId)
  dom    <- hoistMiscErr (note "domain not found" $ m ^? moduleDomains . ix domId)
  let mg  = moduleGraph m
  let gr  = mg ^. moduleGraphGraph
  n      <- hoistMiscErr (note "domain not found" $ mg ^? moduleGraphDomainMap . ix (dom ^. domainId))

  let ts  = getPaths m forwardEdges 10 gr n
  let ps  = getPathSet m ts
  respond (pathSetJSON ps)
