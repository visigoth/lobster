{-# LANGUAGE OverloadedStrings #-}
--
-- Main.hs
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

import Control.Applicative ((<$>))
import Control.Exception
import Data.Aeson ((.=), object)
import Data.Monoid ((<>))

import Lobster.Lexer (alexNoPos)
import Lobster.Error
import Lobster.JSON
import SCD.Lobster.Gen.CoreSyn.Output (showLobster)
import Snap

import V3SPAObject

import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.Encoding  as E
import qualified Data.Text.Lazy.IO        as TIO
import qualified Data.Aeson.Encode.Pretty as AP

import qualified Lobster.Policy           as P
import qualified Version                  as V
import qualified IptablesToLobster        as I

conf :: AP.Config
conf = AP.defConfig
  { AP.confIndent  = 2
  , AP.confCompare = AP.keyOrder
                       [ "name", "class", "args", "ports"
                       , "connections", "subdomains"
                       , "left", "right", "connection"
                       ]
  }

sendVO :: V3SPAObject -> Snap ()
sendVO vo = do
  writeLBS (AP.encodePretty' conf vo)
  writeLBS "\r\n"

sendError :: Error -> Snap ()
sendError err = sendVO $ emptyVO { errors = [buildError err] }

buildError :: Error -> (ErrorLoc, String)
buildError (LocError loc err) = (loc, errorMessage err)
buildError err = (unknownLoc, errorMessage err)

-- | URL handler for "/version".
handleVersion :: Snap ()
handleVersion = method GET $ do
  modifyResponse $ setContentType "application/json"
  let obj = object [ "version" .= V.version ]
  writeLBS (AP.encodePretty obj)
  writeLBS "\r\n"

handleParse :: Snap ()
handleParse = method POST $ do
  modifyResponse $ setContentType "application/json"
  body <- (T.unpack . E.decodeUtf8) <$> readRequestBody 10000000
  let policy = P.parsePolicy body
  case policy of
    Left err -> sendError err
    Right p  -> do
      case P.toDomain p of
        Left err -> sendError err
        Right (checks, dom) -> do
          sendVO $ emptyVO { checkResults = checks
                           , domain = Just dom
                           }

-- | Import IPTables source and output Lobster source or errors.
handleImportIptables :: Snap ()
handleImportIptables = method POST $ do
  modifyResponse $ setContentType "application/json"
  body <- (T.unpack . E.decodeUtf8) <$> readRequestBody 10000000
  case I.toLobster body of
    Left e    -> sendError (MiscError (show e))
    Right lsr -> do
      let obj = object [ "version" .= V.version
                       , "result"  .= (showLobster lsr)
                       , "errors"  .= ([] :: [()])
                       ]
      writeLBS (AP.encodePretty obj)
      writeLBS "\r\n"

site :: Snap ()
site = route
  [ ("/parse", handleParse)
  , ("/version", handleVersion)
  , ("/import/iptables", handleImportIptables)
  ]

main :: IO ()
main = quickHttpServe site
