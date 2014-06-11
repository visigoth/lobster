{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
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
import Data.Text (Text, pack)
import Snap

import Lobster.Core
import V3SPA.Server.Snap

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE

-- | Path query direction.
data GTDirection = GTForward | GTBackward
  deriving (Eq, Ord, Show)

-- | Return the edge function given a direction.
dirEdgeF :: GTDirection -> EdgeF l
dirEdgeF GTForward  = forwardEdges
dirEdgeF GTBackward = backwardEdges

-- | Get the domain ID from query parameters.
paramDomainId :: V3Snap (Maybe DomainId)
paramDomainId = do
  r <- getQueryParam "id"
  return $ maybe Nothing ((DomainId . fst <$>) . BS.readInt) r

-- | Get the query limit from HTTP parameters.
paramLimit :: V3Snap (Maybe Int)
paramLimit = do
  r <- getQueryParam "limit"
  return $ maybe Nothing ((fst <$>) . BS.readInt) r

-- | Get the query direction from HTTP parameters.
paramDirection :: V3Snap (Either (Error l) GTDirection)
paramDirection = do
  r <- getQueryParam "direction"
  case r of
    Nothing -> return (Right GTForward)
    Just x
      | x == "forward"  -> return (Right GTForward)
      | x == "backward" -> return (Right GTBackward)
      | otherwise       -> return (Left (MiscError "invalid direction"))

-- | Parse a comma-separate list of permission names.
parsePerms :: Text -> S.Set Perm
parsePerms t = S.fromList (catMaybes (map parsePerm xs))
  where
    xs = T.splitOn "," t

-- | Get the initial permission filter set from HTTP parameters.
paramPerms :: V3Snap (Maybe (S.Set Perm))
paramPerms = do
  r <- fmap TE.decodeUtf8 <$> getQueryParam "perms"
  return $ fmap parsePerms r

-- | Get the transitive permission filter set from HTTP parameters.
paramTransPerms :: V3Snap (S.Set Perm)
paramTransPerms = do
  r <- fmap TE.decodeUtf8 <$> getQueryParam "trans_perms"
  return $! maybe S.empty parsePerms r

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

-- | "POST /paths?id=N" --- path query
handlePaths :: V3Snap ()
handlePaths = method POST $ do
  modifyResponse $ setContentType "application/json"
  body   <- readRequestBody 10000000
  m      <- hoistErr $ readPolicyBS body

  domId  <- hoistMiscErr =<< (note "parameter 'id' not supplied" <$> paramDomainId)
  dom    <- hoistMiscErr (note "domain not found" $ m ^? moduleDomains . ix domId)
  qdir   <- hoistErr =<< paramDirection
  let f   = dirEdgeF qdir
  limit  <- paramLimit
  let mg  = moduleGraph m
  let gr  = mg ^. moduleGraphGraph
  let mdm = mg ^. moduleGraphDomainMap
  n      <- hoistMiscErr (note "domain not eligible" $ mdm ^? ix (dom ^. domainId))

  perms  <- paramPerms
  tperms <- paramTransPerms
  liftIO $ print tperms
  let (ps, full) = getPaths m f perms tperms 10 limit gr n
  respond (pathSetJSON m ps full)
