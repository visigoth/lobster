{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- Main.hs
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

import Control.Monad.Reader
import Data.Aeson
import Snap

import Lobster.Core

import V3SPA.Server.Snap
import V3SPA.Server.Import.SELinux
import V3SPA.Server.Import.IPTables

----------------------------------------------------------------------
-- Request Handlers

-- | "GET /version" --- request web service version
handleVersion :: V3Snap ()
handleVersion = method GET $ respond Null

-- | "POST /parse" --- parse Lobster to JSON
handleParse :: V3Snap ()
handleParse = method POST $ do
  modifyResponse $ setContentType "application/json"
  body   <- readRequestBody 10000000
  policy <- hoistErr $ parseByteString body
  lsrmod <- hoistErr $ evalPolicy policy
  respond lsrmod

----------------------------------------------------------------------
-- Routing and Main

-- | Routing information for the web service.
site :: V3Snap ()
site = route
  [ ("/parse",           handleParse)
  , ("/version",         handleVersion)
  , ("/import/iptables", handleImportIptables)
  , ("/import/selinux",  handleImportSELinux)
  ]

main :: IO ()
main = do
  let options = defaultOptions
  quickHttpServe (runReaderT (runV3Snap site) options)
