{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- Main.hs
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

import Control.Monad.Reader
import Data.Aeson
import Snap

import V3SPA.Server.Snap
import V3SPA.Server.Paths (handleExportPaths)
import V3SPA.Server.ProjectResource

----------------------------------------------------------------------
-- Request Handlers

-- | "GET /version" --- request web service version
handleVersion :: V3Snap ()
handleVersion = method GET $ respond Null

----------------------------------------------------------------------
-- Routing and Main

-- | Routing information for the web service.
site :: V3Snap ()
site = route
  [ ("/import/iptables",                         handleImportIptables)
  , ("/projects",                                method GET    handleListProjects)
  , ("/projects/:name",                          method POST   handleCreateProject)
  , ("/projects/:name",                          method GET    handleGetProject)
  , ("/projects/:name",                          method DELETE handleDestroyProject)
  , ("/projects/:name/import/selinux",           method POST   handleImportSELinux)
  , ("/projects/:name/modules",                  method POST   handleCreateModules)
  , ("/projects/:name/modules/:module",          method GET    handleGetModule)
  , ("/projects/:name/modules/:module",          method DELETE handleDestroyModule)
  , ("/projects/:name/modules/:module/selinux",  method GET    handleExportSELinux)
  , ("/projects/:name/json",                     method GET    handleExportJson)
  , ("/projects/:name/paths",                    method GET    handleExportPaths)
  , ("/version",                                 handleVersion)
  ]

handler opts = do
  extendTimeout 120
  runReaderT (runV3Snap site) opts

main :: IO ()
main = do
  let options = defaultOptions
  quickHttpServe (handler options)
