{-# LANGUAGE OverloadedStrings #-}
--
-- SELinux.hs --- SELinux exporter for the V3SPA web service.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

module V3SPA.Server.Export.SELinux
  ( handleExportSELinux
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (toJSON)
import Snap

import Lobster.Core
import Lobster.SELinux
import V3SPA.Server.Snap

-- | "POST /export/selinux" --- export Lobster to SELinux
handleExportSELinux :: V3Snap ()
handleExportSELinux = method POST $ do
  modifyResponse $ setContentType "application/json"
  body   <- readBody
  liftIO $ print body
  m      <- hoistErr $ readPolicyBS body
  let pol = exportSELinux m
  respond (toJSON pol)
