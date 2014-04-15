{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
--
-- SELinux.hs --- SELinux importer for the V3SPA web service.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module V3SPA.Server.Import.SELinux
  ( handleImportSELinux
  ) where

import Control.Error
import Control.Monad.Reader
import Data.Aeson
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import SCD.M4.ModuleFiles
import Snap

import CoreSyn (showLobster)
import V3SPA.Server.Snap

import qualified SCD.M4.Syntax            as M4
import qualified M4ToLobster              as M

data SELinuxImportRequest = SELinuxImportRequest
  { seReqRefpolicy :: String
  , seReqModules   :: [ModuleSource]
  } deriving Show

instance FromJSON SELinuxImportRequest where
  parseJSON (Object v) =
    SELinuxImportRequest <$> v .: "refpolicy"
                         <*> v .: "modules"
  parseJSON _ = mzero

-- XXX kind of lousy orphan instance here
instance FromJSON ModuleSource where
  parseJSON (Object v) =
    ModuleSource <$> v .: "name"
                 <*> v .: "if"
                 <*> v .: "te"
                 <*> v .: "fc"
  parseJSON _ = mzero

importModule :: M4.Policy -> ModuleSource -> V3Snap M4.Policy
importModule p modSrc = do
  m <- hoistMiscErr $ readPolicyModuleSource modSrc
  return $ addPolicyModule m p

-- | "POST /import/selinux" --- import SELinux to Lobster
handleImportSELinux :: V3Snap ()
handleImportSELinux = method POST $ do
  modifyResponse $ setContentType "application/json"
  body    <- readRequestBody 10000000
  req     <- hoistMiscErr $ note "malformed JSON request" $ decode body
  refPath <- refPolicyDir (seReqRefpolicy req)
  policy0 <- liftIO $ readPolicy Nothing refPath
  policy1 <- foldM importModule policy0 (seReqModules req)
  let subAttrFile = refPath ++ "/subattributes"
  ok      <- liftIO $ doesFileExist subAttrFile
  subAttr <- liftIO $
    if not ok then return []
    else fmap M.parseSubAttributes (readFile subAttrFile)
  lsr     <- hoistMiscErr (fmapL show $ M.toLobster M.Mode3 subAttr policy1)
  respond (showLobster lsr)

-- | Return the directory that contains reference policy versions.
refPolicyBaseDir :: V3Snap FilePath
refPolicyBaseDir = do
  dataDir <- fromMaybe "." <$> asks optionDataDir
  return $ dataDir </> "refpolicy"

-- | Return the directory for a reference policy by name.
refPolicyDir :: String -> V3Snap FilePath
refPolicyDir name = (</> name) <$> refPolicyBaseDir
