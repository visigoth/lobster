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
import Data.List (isPrefixOf, isSuffixOf)
import Data.String (fromString)
import Data.Text (Text)
import System.FilePath ((</>), (<.>), makeRelative, takeDirectory)
import System.Directory ( createDirectoryIfMissing
                        , doesDirectoryExist
                        , doesFileExist
                        , getDirectoryContents
                        , removeDirectoryRecursive
                        )

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text

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
  items <- listDirectory basePath
  dirs  <- filterM isDirectory items
  return $ ProjectList (project . Text.pack <$> dirs)
  where
    isDirectory dir = let path = basePath </> fromString dir
                      in liftIO $ doesDirectoryExist path

createProject :: MonadIO m => Text -> m (Maybe Project)
createProject name = do
  exists <- isProjectDir name
  if exists
  then return Nothing
  else do
    let p = project name & projectModules .~ Just []
    liftIO $ createDirectoryIfMissing True (projectPath p)
    return (Just p)

getProject :: (Functor m, MonadIO m) => Text -> m (Maybe Project)
getProject name = do
  exists <- isProjectDir name
  if exists
  then do
    let p = project name
    moduleFiles <- findFilesRecursive (return . isLobster) [projectPath p]
    let moduleNames = makeRelative (projectPath p) <$> moduleFiles
    let modules     = Module . fromString <$> moduleNames
    return $ Just $ project name & projectModules .~ Just modules
  else return Nothing

destroyProject :: (Functor m, MonadIO m) => Text -> m (Maybe ())
destroyProject name = do
  let path = projectPath (project name)
  exists <- liftIO $ doesDirectoryExist path
  if exists
  then Just <$> liftIO (removeDirectoryRecursive path)
  else return Nothing

isProjectDir :: MonadIO m => Text -> m Bool
isProjectDir name = let path = basePath </> Text.unpack name
                    in liftIO $ doesDirectoryExist path

isLobster :: FilePath -> Bool
isLobster = isSuffixOf ".lsr"

putModule :: MonadIO m => Module -> LBS.ByteString -> Project -> m ()
putModule m source p = do
  let path = modulePath m p
  let dir  = takeDirectory path
  liftIO $ createDirectoryIfMissing True dir
  liftIO $ LBS.writeFile path source

getModuleSource :: (Functor m, MonadIO m) => Module -> Project -> m (Maybe LBS.ByteString)
getModuleSource m p = do
  let path = modulePath m p
  exists <- liftIO $ doesFileExist path
  case exists of
    True  -> Just <$> liftIO (LBS.readFile path)
    False -> return Nothing

basePath :: FilePath
basePath = "projects"

projectPath :: Project -> FilePath
projectPath p = basePath </> Text.unpack (p ^. projectName)

-- TODO: Handle path separators in module name
modulePath :: Module -> Project -> FilePath
modulePath m p = projectPath p </> Text.unpack (m ^. moduleName)  <.> "lsr"

-- | List all entries in a directory except for entries beginning with `.`
listDirectory :: MonadIO m => FilePath -> m [FilePath]
listDirectory path = do
  items <- liftIO $ getDirectoryContents path
  return $ filter (not . isPrefixOf ".") items

findFilesRecursive :: (Functor m, MonadIO m)
                   => (FilePath -> m Bool) -> [FilePath] -> m [FilePath]
findFilesRecursive predicate (d:ds) = do
  matches <- predicate d
  isDir   <- liftIO $ doesDirectoryExist d
  nested  <- if isDir
             then do
               items <- listDirectory d
               return $ (d </>) <$> items
             else return []
  let rec = findFilesRecursive predicate (nested ++ ds)
  if matches
  then (d:) <$> rec
  else rec
findFilesRecursive _ [] = return []
