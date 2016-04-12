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
import Data.ByteString (ByteString)
import Data.List (isPrefixOf, isSuffixOf)
import Data.String (fromString)
import System.FilePath ((</>), (<.>), makeRelative, splitExtension, takeDirectory)
import System.Directory ( copyFile
                        , createDirectoryIfMissing
                        , doesDirectoryExist
                        , doesFileExist
                        , getDirectoryContents
                        , removeDirectoryRecursive
                        )

import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as LBS

data Project = Project { _projectName    :: ByteString
                       , _projectModules :: Maybe [Module]
                       }
newtype ProjectList = ProjectList { _projectList :: [Project] }
newtype Module      = Module      { _moduleName  :: ByteString }

makeLenses ''Project
makeLenses ''Module

instance ToJSON Project where
  toJSON p = case p ^. projectModules of
    Just ms -> object [ "name"    .= BS.toString (p ^. projectName)
                      , "modules" .= ms
                      ]
    Nothing -> object [ "name" .= BS.toString (p ^. projectName) ]

instance ToJSON ProjectList where
  toJSON (ProjectList ps) = object [ "items" .= ps ]

instance ToJSON Module where
  toJSON m = object [ "name" .= BS.toString (m ^. moduleName) ]

mkProject :: ByteString -> Project
mkProject name = Project { _projectName = name, _projectModules = Nothing }

mkModule :: ByteString -> Module
mkModule name = Module { _moduleName = nameWithExtension }
  where
    nameWithExtension = if extension == ".lsr"
                        then fromString fileName
                        else name
    (fileName, extension) = splitExtension (BS.toString name)

listProjects :: MonadIO m => m ProjectList
listProjects = do
  items <- listDirectory basePath
  dirs  <- filterM isDirectory items
  return $ ProjectList (mkProject . fromString <$> dirs)
  where
    isDirectory dir = let path = basePath </> fromString dir
                      in liftIO $ doesDirectoryExist path

createProject :: MonadIO m => ByteString -> m (Maybe Project)
createProject name = do
  exists <- isProjectDir name
  if exists
  then return Nothing
  else do
    let p = mkProject name & projectModules .~ Just []
    liftIO $ createDirectoryIfMissing True (projectPath p)
    return (Just p)

getProject :: (Functor m, MonadIO m) => ByteString -> m (Maybe Project)
getProject name = do
  exists <- isProjectDir name
  if exists
  then do
    let p = mkProject name
    moduleFiles <- findFilesRecursive (return . isLobster) [projectPath p]
    let moduleNames = makeRelative (projectPath p) <$> moduleFiles
    let modules     = Module . fromString <$> moduleNames
    return $ Just $ mkProject name & projectModules .~ Just modules
  else return Nothing

destroyProject :: (Functor m, MonadIO m) => ByteString -> m (Maybe ())
destroyProject name = do
  let path = projectPath (mkProject name)
  exists <- liftIO $ doesDirectoryExist path
  if exists
  then Just <$> liftIO (removeDirectoryRecursive path)
  else return Nothing

isProjectDir :: MonadIO m => ByteString -> m Bool
isProjectDir name = let path = basePath </> BS.toString name
                    in liftIO $ doesDirectoryExist path

isLobster :: FilePath -> Bool
isLobster = isSuffixOf ".lsr"

putModule :: MonadIO m => Module -> FilePath -> Project -> m ()
putModule m source p = do
  let path = modulePath m p
  let dir  = takeDirectory path
  liftIO $ createDirectoryIfMissing True dir
  liftIO $ copyFile source path

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
projectPath p = basePath </> BS.toString (p ^. projectName)

-- TODO: Handle path separators in module name
modulePath :: Module -> Project -> FilePath
modulePath m p = projectPath p </> BS.toString (m ^. moduleName)  <.> "lsr"

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
