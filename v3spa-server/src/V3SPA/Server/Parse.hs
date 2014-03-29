{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- Parse.hs --- Parsing Lobster to JSON for V3SPA.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module V3SPA.Server.Parse
  ( handleParse
  ) where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Read (decimal)
import Snap
import Lobster.Core

import V3SPA.Server.Snap

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding    as TE

maybeM_ :: Monad m => (a -> m ()) -> Maybe a -> m ()
maybeM_ f x = maybe (return ()) f x

----------------------------------------------------------------------
-- Parse Options

-- | Parser options from query parameters.
data ParserOptions = ParserOptions
  { optDepth :: Maybe Int
  , optPaths :: [Text]
  , optIds   :: [DomainId]
  } deriving Show

{-
-- | Default options with no filters set.
defaultOptions :: ParserOptions
defaultOptions = ParserOptions
  { optDepth = Nothing
  , optPaths = []
  , optIds   = []
  }
-}

-- | Return true if no options are set.
isNoDomainPred :: ParserOptions -> Bool
isNoDomainPred (ParserOptions Nothing [] []) = True
isNoDomainPred _ = False

-- | Get a list of text values for a query parameter.
getTextParams :: BS.ByteString -> V3Snap [Text]
getTextParams name = do
  r <- rqQueryParam name <$> getRequest
  return $ maybe [] (map TE.decodeUtf8) r

-- | Convert a string to a domain ID.
readDomainId :: Text -> Maybe DomainId
readDomainId t =
  case decimal t of
    Right (x, _) -> Just (DomainId x)
    _            -> Nothing

-- | Get the maximum depth from query parameters.
getMaxDepth :: V3Snap (Maybe Int)
getMaxDepth = do
  r <- getQueryParam "maxdepth"
  return $ maybe Nothing ((fst <$>) . BS.readInt) r

-- | Get the list of filter domain paths from query parameters.
getPaths :: V3Snap [Text]
getPaths = getTextParams "path"

-- | Get the list of filter domain ids from query parameters.
getIds :: V3Snap [DomainId]
getIds = (catMaybes . map readDomainId) <$> getTextParams "id"

-- | Get parser options from the request.
getParserOptions :: V3Snap ParserOptions
getParserOptions =
  ParserOptions <$> getMaxDepth
                <*> getPaths
                <*> getIds

-- | Build a filter predicate from query parameters.
queryPred :: Module l -> V3Snap (DomainPred l)
queryPred m = do
  opts <- getParserOptions
  if isNoDomainPred opts
    then return noDomainPred
    else queryPredFromOpts m opts

-- | Build a filter predicate from parser options.
queryPredFromOpts :: Module l -> ParserOptions -> V3Snap (DomainPred l)
queryPredFromOpts m opts =
  return $ runDomainPredBuilder $ do
    maybeM_ (addPred . maxDepth m)   (optDepth opts)
    mapM_   (addPred . isDomainPath) (optPaths opts)
    mapM_   (addPred . isDomainId)   (optIds opts)

-- | "POST /parse" --- parse Lobster to JSON
handleParse :: V3Snap ()
handleParse = method POST $ do
  modifyResponse $ setContentType "application/json"
  body   <- readRequestBody 10000000
  policy <- hoistErr $ parseByteString body
  lsrmod <- hoistErr $ evalPolicy policy
  p      <- queryPred lsrmod
  respond (moduleJSON lsrmod p)
