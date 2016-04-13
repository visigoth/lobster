{-# LANGUAGE OverloadedStrings #-}

module V3SPA.Server.ProjectResource where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.String (fromString)
import Snap
import System.Directory (getTemporaryDirectory)

import V3SPA.Server.Project
import V3SPA.Server.Snap

import qualified Snap.Util.FileUploads as F

handleListProjects :: MonadSnap m => m ()
handleListProjects = do
  ps <- listProjects
  respondOk
  respond ps

handleCreateProject :: MonadSnap m => m ()
handleCreateProject = do
  name <- getRequiredParam "name"
  proj <- createProject name
  -- TODO: handle case where proj is Nothing, when project already exists
  respondOk
  respond proj

handleImportSELinux = undefined
handleImportIptables = undefined

handleGetProject :: MonadSnap m => m ()
handleGetProject = do
  name      <- getRequiredParam "name"
  maybeProj <- getProject name
  case maybeProj of
    Just proj -> do
      respondOk
      respond proj
    Nothing -> modifyResponse $ setResponseCode 404

handleUpdateProject = undefined

handleDestroyProject :: MonadSnap m => m ()
handleDestroyProject = do
  name   <- getRequiredParam "name"
  result <- destroyProject name
  case result of
    Just _  -> modifyResponse $ setResponseCode 204
    Nothing -> modifyResponse $ setResponseCode 404

handleExportSELinux = undefined

handleCreateModules :: MonadSnap m => m ()
handleCreateModules = do
  requireContentType formMultipart
  projName <- getRequiredParam "name"
  project  <- createProject projName
  tempDir  <- liftIO getTemporaryDirectory
  F.handleFileUploads tempDir uploadPolicy partUploadPolicy (onFile project)
  updated  <- createProject projName  -- get project again to get updated modules list
  respondOk
  respond updated
  where
    onFile :: (MonadSnap m)
           => Project -> [(F.PartInfo, Either F.PolicyViolationException FilePath)] -> m ()
    onFile _       [] = return ()
    onFile project ((info, Right tempPath):fs) =
      case F.partFileName info of
        Just fileName -> do
          putModule (mkModule fileName) tempPath project
          onFile project fs
        Nothing -> reportFilenameRequirement
    onFile _ ((_, Left policyViolation):_) = reportPolicyViolation policyViolation


handleGetModule :: (Functor m, MonadSnap m) => m ()
handleGetModule = do
  projName <- getRequiredParam "name"
  modName  <- getRequiredParam "module"
  content  <- getModuleSource (mkModule modName) (mkProject projName)
  case content of
    Just bs -> do
      respondOk
      setContentType' lobsterType
      writeLBS bs
    Nothing -> do
      modifyResponse $ setResponseCode 404

handleDestroyModule = undefined
handleExportJson = undefined
handleExportPaths = undefined


----------------------------------------------------------------------
-- helpers

uploadPolicy :: F.UploadPolicy
uploadPolicy = F.setProcessFormInputs False F.defaultUploadPolicy

partUploadPolicy :: F.PartInfo -> F.PartUploadPolicy
partUploadPolicy _ = F.allowWithMaximumSize $ 100 * 1024 * 1024  -- 100 MB

reportPolicyViolation :: MonadSnap m => F.PolicyViolationException -> m ()
reportPolicyViolation policyViolation = do
  modifyResponse $ setResponseCode 400
  writeBS (fromString (show policyViolation))
  getResponse >>= finishWith

reportFilenameRequirement :: MonadSnap m => m ()
reportFilenameRequirement = do
  modifyResponse $ setResponseCode 400
  writeBS "A filename is required for each uploaded file"
  getResponse >>= finishWith
