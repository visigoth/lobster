{-# LANGUAGE OverloadedStrings #-}

module V3SPA.Server.ProjectResource where

import Snap

import V3SPA.Server.Project
import V3SPA.Server.Snap

handleListProjects :: MonadSnap m => m ()
handleListProjects = do
  ps <- listProjects
  respondOk
  respond ps

handleCreateProject :: MonadSnap m => m ()
handleCreateProject = do
  name <- getTextParam "name"
  proj <- createProject name
  -- TODO: handle case where proj is Nothing, when project already exists
  respondOk
  respond proj

handleImportSELinux = undefined
handleImportIptables = undefined

handleGetProject :: MonadSnap m => m ()
handleGetProject = do
  name      <- getTextParam "name"
  maybeProj <- getProject name
  case maybeProj of
    Just proj -> do
      respondOk
      respond proj
    Nothing -> modifyResponse $ setResponseCode 404

handleUpdateProject = undefined

handleDestroyProject :: MonadSnap m => m ()
handleDestroyProject = do
  name   <- getTextParam "name"
  result <- destroyProject name
  case result of
    Just _  -> modifyResponse $ setResponseCode 204
    Nothing -> modifyResponse $ setResponseCode 404

handleExportSELinux = undefined
handleCreateModule = undefined
handleGetModule = undefined
handleDestroyModule = undefined
handleExportJson = undefined
handleExportPaths = undefined
