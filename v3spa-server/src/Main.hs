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

import Lobster.Lexer (alexNoPos)
import Lobster.Error
import Lobster.JSON
import Snap

import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.Encoding  as E
import qualified Data.Text.Lazy.IO        as TIO

import qualified Data.Aeson.Encode.Pretty as AP

import qualified Lobster.Policy           as P

import V3SPAObject

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
sendError err = sendVO $ emptyVO { errors = buildErrors err }

buildErrors err = map go (errorMessage err)
  where
    go s = (alexNoPos, s)   -- FIXME: add position

handleParse :: Snap ()
handleParse = method POST $ do
  body <- (T.unpack . E.decodeUtf8) <$> readRequestBody 10000000
  liftIO $ putStrLn $ "body: " ++ show body
  let policy = P.parsePolicy body
  modifyResponse $ setContentType "application/json"
  case policy of
    Left err -> sendError err
    Right p  -> do
      liftIO $ putStrLn (show p)
      case P.toDomain p of
        Left err -> sendError err
        Right (checks, dom) -> do
          sendVO $ emptyVO { checkResults = checks
                           , domain = Just dom
                           }

site :: Snap ()
site = route [("/parse", handleParse)]

main :: IO ()
main = quickHttpServe site
