{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
--
-- Paths.hs --- Path queries on a Lobster module.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

module V3SPA.Server.Paths
  ( handleExportPaths
  ) where

import Control.Applicative ((<$>))
import Control.Error
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Text (Text, pack)
import Snap

import Lobster.Core
import V3SPA.Server.Project hiding (Module)
import V3SPA.Server.Snap

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE

-- | Get the domain ID from query parameters.
paramDomainId :: BS.ByteString -> Either String DomainId
paramDomainId param =
  let maybeId = (DomainId . fst ) <$> BS.readInt param
  in note "Parameter 'id' must be an integer." maybeId

-- | Get the query limit from HTTP parameters.
paramLimit :: Maybe BS.ByteString -> Either String (Maybe Int)
paramLimit maybeParam = note "Parameter 'limit' must be an integer." $
  case maybeParam of
    Just param -> Just . fst <$> BS.readInt param
    Nothing    -> Just Nothing

-- | Get the query direction from HTTP parameters.
paramDirection :: Maybe BS.ByteString -> Either String GTDirection
paramDirection param =
  case param of
    Nothing -> Right GTForward
    Just x
      | x == "forward"  -> Right GTForward
      | x == "backward" -> Right GTBackward
      | otherwise       -> Left "invalid direction"

-- | Parse a comma-separate list of permission names.
parsePerms :: Text -> S.Set Perm
parsePerms t = S.fromList (catMaybes (map parsePerm xs))
  where
    xs = T.splitOn "," t

-- | Get the initial permission filter set from HTTP parameters.
paramPerms :: Maybe BS.ByteString -> Either String (Maybe (S.Set Perm))
paramPerms maybeParam = Right (parsePerms . TE.decodeUtf8 <$> maybeParam)

-- | Get the transitive permission filter set from HTTP parameters.
paramTransPerms :: Maybe BS.ByteString -> Either String (S.Set Perm)
paramTransPerms maybeParam = Right $
  case maybeParam of
    Just param -> parsePerms (TE.decodeUtf8 param)
    Nothing    -> S.empty

-- | Output a path node as JSON.
pathNodeJSON :: Module l -> GTNode -> Value
pathNodeJSON m node =
  object [ "conn"  .= connId
         , "left"  .= leftDomId
         , "right" .= rightDomId
         ]
  where
    conn       = node ^. gtnodeConn
    connId     = getConnKey (conn ^. gconnId)
    leftDomId  = getDomKey (m ^. idPort (conn ^. gconnLeft)  . portDomain)
    rightDomId = getDomKey (m ^. idPort (conn ^. gconnRight) . portDomain)

-- | Output a single path as JSON.
pathJSON :: Module l -> [GTNode] -> Value
pathJSON m xs = toJSON (map (pathNodeJSON m) xs)

-- | Output a path set as JSON.
pathSetJSON :: Module l -> PathSet -> Bool -> Value
pathSetJSON m ps full =
  object $ [ (pack $ show domId) .= (map (pathJSON m) (S.toList s))
           | (DomainId domId, s) <- M.toList (getPathSet ps)
           ] ++ ["truncated" .= not full]

{-
-- | Helper to filter non-type domains from a path set.
isType :: Module l -> DomainId -> a -> Bool
isType m domId _ = isJust ann
  where
    dom = m ^. idDomain domId
    ann = lookupAnnotation "Type" (dom ^. domainAnnotation)
-}

-- | Concatenate module sources and extract information flow paths.
handleExportPaths :: MonadSnap m => m ()
handleExportPaths = do
  projName <- getRequiredParam                   "name"
  domId    <- parseRequiredParam paramDomainId   "id"
  qdir     <- parseParam         paramDirection  "direction"
  limit    <- parseParam         paramLimit      "limit"
  perms    <- parseParam         paramPerms      "perms"
  tperms   <- parseParam         paramTransPerms "trans_perms"
  let proj = mkProject projName
  lobsterSource <- (handleError 404 . note ("Project not found." :: String)) =<< getConcatenatedModuleSources proj

  m      <- hoistErr $ readPolicyBS lobsterSource
  dom    <- hoistMiscErr (note "domain not found" $ m ^? moduleDomains . ix domId)
  let mg  = moduleGraph m
  let gr  = mg ^. moduleGraphGraph
  let mdm = mg ^. moduleGraphDomainMap
  n      <- hoistMiscErr (note "domain not eligible" $ mdm ^? ix (dom ^. domainId))

  let (ps, full) = getPaths m qdir perms tperms 10 limit gr n
  let paths      = pathSetJSON m ps full
  respondOk
  respond paths
