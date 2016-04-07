{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
--
-- Project.hs --- Manage saving project files to filesystem & reading files back.
--
-- Copyright (C) 2016, Galois, Inc.
-- All Rights Reserved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

module V3SPA.Server.Project where

import Control.Applicative ((<$>))
import Control.Lens hiding ((.=), (<.>))
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Text (Text)
import Filesystem.Path.CurrentOS (FilePath, (</>), (<.>))
import System.Directory ( createDirectoryIfMissing
                        , doesDirectoryExist
                        , doesFileExist
                        , getDirectoryContents
                        )
import Prelude hiding (FilePath)

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path

data Project = Project { _projectName    :: Text
                       , _projectModules :: Maybe [Module]
                       }
newtype ProjectList = ProjectList { _projectList :: [Project] }
newtype Module      = Module      { _moduleName  :: Text }

makeLenses ''Project
makeLenses ''Module

instance ToJSON Project where
  toJSON p = case p ^. projectModules of
    Just ms -> object [ "name"    .= (p ^. projectName)
                      , "modules" .= ms
                      ]
    Nothing -> object [ "name" .= (p ^. projectName) ]

instance ToJSON ProjectList where
  toJSON (ProjectList ps) = object [ "items" .= ps ]

instance ToJSON Module where
  toJSON m = object [ "name" .= (m ^. moduleName) ]

project :: Text -> Project
project name = Project { _projectName = name, _projectModules = Nothing }

listProjects :: MonadIO m => m ProjectList
listProjects = do
  allItems <- liftIO $ getDirectoryContents (Path.encodeString basePath)
  let items = filter ((/= '.') . head) allItems
  dirs <- liftIO $ filterM doesDirectoryExist items
  return $ ProjectList (project . Text.pack <$> dirs)

putModule :: MonadIO m => Module -> LBS.ByteString -> Project -> m ()
putModule m source p = do
  let path = modulePath m p
  let dir  = Path.directory path
  liftIO $ createDirectoryIfMissing True (Path.encodeString dir)
  liftIO $ LBS.writeFile (Path.encodeString path) source

getModuleSource :: (Functor m, MonadIO m) => Module -> Project -> m (Maybe LBS.ByteString)
getModuleSource m p = do
  let path = modulePath m p
  exists <- liftIO $ doesFileExist (Path.encodeString path)
  case exists of
    True  -> Just <$> liftIO (LBS.readFile (Path.encodeString path))
    False -> return Nothing

basePath :: FilePath
basePath = "projects"

projectPath :: Project -> FilePath
projectPath p = basePath </> Path.fromText (p ^. projectName)

-- TODO: Handle path separators in module name
modulePath :: Module -> Project -> FilePath
modulePath m p = projectPath p </> Path.fromText (m ^. moduleName)  <.> "lsr"
