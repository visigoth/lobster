{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- Snap.hs --- Snap utilities for the V3SPA web service.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

module V3SPA.Server.Snap where

import Control.Applicative (Applicative, Alternative, (<$>))
import Control.Error
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Reader
import Data.Aeson
import Snap

import Lobster.Core (Span(..), Error(..))
import V3SPA.Server.Version

import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.Encoding  as E
import qualified Data.Aeson.Encode.Pretty as AP

-- | Snap monad extended with a "Reader" for configuration data.
--
-- TODO: Add something here to collect warnings.
newtype V3Snap a = V3Snap { runV3Snap :: ReaderT Options Snap a }
  deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
           , MonadIO, MonadCatchIO, MonadSnap, MonadReader Options)

-- | Read the request body with a 100MiB limit.
readBody = readRequestBody (100 * 1024 * 1024)

-- | Read the request body as a string.
readBodyString :: V3Snap String
readBodyString = T.unpack . E.decodeUtf8 <$> readBody

-- | Encode a result as JSON and write it to the response.
writeJSON :: ToJSON a => a -> V3Snap ()
writeJSON x = do
  writeLBS (AP.encodePretty' conf x)
  writeLBS "\r\n"

----------------------------------------------------------------------
-- Response Handling

-- | Create a response object given a result and list of errors.
mkResp :: ToJSON a => a -> [Error Span] -> Value
mkResp result errors =
  object [ "version" .= version
         , "result"  .= result
         , "errors"  .= errors
         ]

-- | Send a successful response.
respond :: ToJSON a => a -> V3Snap ()
respond x = writeJSON (mkResp x [])

-- | Send an error response as JSON from an error object.
sendError :: Value -> V3Snap a
sendError obj = do
  writeJSON obj
  r <- getResponse
  finishWith r

-- | Run an action that may fail in the "V3Snap" monad,
-- catching errors and returning them as a JSON response.
hoistErr :: Either (Error Span) a -> V3Snap a
hoistErr (Left e)  = sendError (mkResp Null [e])
hoistErr (Right x) = return x

-- | Run an action that may fail with a string in the
-- "V3Snap" monad, catching errors and returning them
-- as a JSON response.
hoistMiscErr :: Either String a -> V3Snap a
hoistMiscErr = hoistErr . fmapL MiscError

-- | Pretty JSON configuration for parsed Lobster.
conf :: AP.Config
conf = AP.defConfig
  { AP.confIndent  = 2
  , AP.confCompare = AP.keyOrder
                       [ "name", "class", "args", "ports"
                       , "connections", "subdomains"
                       , "left", "right", "connection"
                       ]
  }

----------------------------------------------------------------------
-- Option Processing

-- | Web service options (from command line).
data Options = Options
  { optionDataDir :: Maybe FilePath
  } deriving Show

-- | Default options.
defaultOptions :: Options
defaultOptions = Options
  { optionDataDir = Nothing
  }
