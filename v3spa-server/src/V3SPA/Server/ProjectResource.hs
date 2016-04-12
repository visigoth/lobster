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
  projName  <- getRequiredParam "name"
  maybeProj <- getProject projName
  proj      <- case maybeProj of
    Just proj -> return proj
    Nothing   -> do
      modifyResponse $ setResponseCode 404
      getResponse >>= finishWith
  tempDir  <- liftIO getTemporaryDirectory
  modCount <- F.handleFileUploads tempDir uploadPolicy partUploadPolicy (onFile proj 0)
  respondOk
  writeBS ("Recived " <> fromString (show modCount) <> " modules")
  where
    onFile :: (MonadSnap m, Num n)
           => Project -> n -> [(F.PartInfo, Either F.PolicyViolationException FilePath)] -> m n
    onFile _    count [] = return count
    onFile proj count ((info, Right tempPath):fs) =
      case F.partFileName info of
        Just fileName -> do
          putModule (mkModule fileName) tempPath proj
          onFile proj (count + 1) fs
        Nothing -> do
          reportFilenameRequirement
          return 0
    onFile _ _ ((_, Left policyViolation):_) = do
      reportPolicyViolation policyViolation
      return 0


handleGetModule = undefined
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
