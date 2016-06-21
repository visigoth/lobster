{-# LANGUAGE OverloadedStrings #-}

module V3SPA.Server.ProjectResource where

import Control.Error (note)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(Null), decode)
import Data.Monoid ((<>))
import Data.String (fromString)
import Snap
import System.Directory (getTemporaryDirectory)

import Lobster.Core (readPolicyBS, moduleJSON)
import Lobster.SELinux (exportSELinux)
import V3SPA.Server.Import.SELinux (importModules)
import V3SPA.Server.Project
import V3SPA.Server.Snap
import V3SPA.Server.Parse (queryPred)

import qualified Data.ByteString.Lazy  as LBS
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
  respondOk
  respond proj

handleImportSELinux :: V3Snap ()
handleImportSELinux = do
  projName <- getRequiredParam "name"
  body     <- readBody
  req      <- hoistMiscErr $ note "malformed JSON request" $ decode body
  project  <- createProject projName
  lsr      <- importModules req
  let m = mkModule ("imported/selinux")
  putModule m lsr project
  respondCreated ("/projects/" <> projName <> "/modules/imported%2Fselinux")
  respond Null

handleGetProject :: MonadSnap m => m ()
handleGetProject = do
  name      <- getRequiredParam "name"
  maybeProj <- getProject name
  case maybeProj of
    Just proj -> do
      respondOk
      respond proj
    Nothing -> do
      modifyResponse $ setResponseCode 404
      hoistMiscErr (Left "project not found")

handleDestroyProject :: MonadSnap m => m ()
handleDestroyProject = do
  name   <- getRequiredParam "name"
  result <- destroyProject name
  case result of
    Just _  -> modifyResponse $ setResponseCode 204
    Nothing -> modifyResponse $ setResponseCode 404
  respond Null

handleExportSELinux :: MonadSnap m => m ()
handleExportSELinux = do
  projName <- getRequiredParam "name"
  modName  <- getRequiredParam "module"
  content  <- getModuleSource (mkModule modName) (mkProject projName)
  case content of
    Just bs -> do
      m <- hoistErr $ readPolicyBS bs
      respondOk
      respond (exportSELinux m)
    Nothing -> do
      modifyResponse $ setResponseCode 404
      hoistMiscErr (Left "Project or module not found.")

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
    onFile :: MonadSnap m
           => Project -> [(F.PartInfo, Either F.PolicyViolationException FilePath)] -> m ()
    onFile project ((info, Right filePath):fs) = do
      fileName <- case F.partFileName info of
        Just name -> return name
        Nothing -> reportFilenameRequirement >> return "(anon)"
      source <- liftIO $ LBS.readFile filePath
      putModule (mkModule fileName) source project
      onFile project fs
    onFile _       [] = return ()
    onFile _ ((_, Left policyViolation):_) = reportPolicyViolation policyViolation

handleGetModule :: MonadSnap m => m ()
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

handleDestroyModule :: MonadSnap m => m ()
handleDestroyModule = do
  projName <- getRequiredParam "name"
  modName  <- getRequiredParam "module"
  result   <- destroyModule (mkModule modName) (mkProject projName)
  case result of
    Just _  -> modifyResponse $ setResponseCode 204
    Nothing -> modifyResponse $ setResponseCode 404

-- | Export the project in JSON format for visualization.
handleExportJson :: MonadSnap m => m ()
handleExportJson = do
  projName <- getRequiredParam "name"
  let proj = mkProject projName
  lobsterSource <- (handleError 404 . note ("Project not found." :: String)) =<< getConcatenatedModuleSources proj
  m <- hoistErr $ readPolicyBS lobsterSource
  p <- queryPred m
  respondOk
  respond (moduleJSON m p)

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
