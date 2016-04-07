module V3SPA.Server.ProjectResource where

import Snap

import V3SPA.Server.Project
import V3SPA.Server.Snap

handleListProjects :: MonadSnap m => m ()
handleListProjects = do
  ps <- listProjects
  respondOk
  respond ps


handleCreateProject = undefined
handleImportSELinux = undefined
handleImportIptables = undefined
handleGetProject = undefined
handleUpdateProject = undefined
handleDestroyProject = undefined
handleExportSELinux = undefined
handleCreateModule = undefined
handleGetModule = undefined
handleDestroyModule = undefined
handleExportJson = undefined
handleExportPaths = undefined
