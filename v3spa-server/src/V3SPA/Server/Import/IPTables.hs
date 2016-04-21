{-# LANGUAGE OverloadedStrings #-}
--
-- IPTables.hs --- Import IPTables to Lobster.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

module V3SPA.Server.Import.IPTables
  ( handleImportIptables
  ) where

import Control.Error
import Snap

import Lobster.Core
import CoreSyn (showLobster)

import V3SPA.Server.Snap

import qualified CoreSyn                  as L
import qualified IptablesToLobster        as I

-- | Parse IPtables from a request body string.
importIptables :: String -> Either (Error Span) [L.Decl]
importIptables = fmapL (MiscError . show) . I.toLobster

-- | "POST /import/iptables" --- import IPTables to Lobster
handleImportIptables :: V3Snap ()
handleImportIptables = method POST $ do
  modifyResponse $ setContentType "application/json"
  body <- readBodyString
  lsr  <- hoistErr $ importIptables body
  respondOk
  respond (showLobster lsr)
