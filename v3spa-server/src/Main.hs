{-# LANGUAGE OverloadedStrings #-}
--
-- Main.hs
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

import Control.Applicative ((<$>))
import Control.Exception
import Data.Monoid ((<>))

import Lobster.Monad (runP)
import Lobster.JSON
import Snap

import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.Encoding  as E
import qualified Data.Text.Lazy.IO        as TIO

import qualified Data.Aeson.Encode.Pretty as AP

import qualified Lobster.Policy           as P

conf :: AP.Config
conf = AP.defConfig
  { AP.confIndent  = 2
  , AP.confCompare = AP.keyOrder
                       [ "name", "class", "args", "ports"
                       , "connections", "subdomains"
                       , "left", "right", "connection"
                       ]
  }

errorHandler :: ErrorCall -> IO (Either String a)
errorHandler (ErrorCall s) = return (Left s)

-- | Interpret a policy, returning the domain or an error
-- string.  We have to go to some trouble here because the
-- interpreter just bombs with "error" if there is an error.
interpret :: P.Policy -> IO (Either String P.Domain)
interpret p = catch (return . Right . snd $ P.interpretPolicy p) errorHandler

handleParse :: Snap ()
handleParse = method POST $ do
  body <- (T.unpack . E.decodeUtf8) <$> readRequestBody 10000000
  let policy = P.parsePolicy body
  case policy of
    -- TODO: Better error reporting!
    Left err -> do
      modifyResponse $ setContentType "text/plain"
      modifyResponse $ setResponseCode 500
      writeLazyText (T.pack err)
    Right p  -> do
      let result = runP (P.toDomain p)
      case result of
        Left err -> do
          modifyResponse $ setContentType "text/plain"
          modifyResponse $ setResponseCode 500
          writeLazyText (T.pack err)
        Right domain -> do
          modifyResponse $ setContentType "application/json"
          writeLBS (AP.encodePretty' conf domain)
          writeLBS "\r\n"

site :: Snap ()
site = route [("/parse", handleParse)]

main :: IO ()
main = quickHttpServe site
