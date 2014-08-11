{-# LANGUAGE OverloadedStrings #-}
--
-- lobster-json.hs --- Export a Lobster file in JSON format.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

import Control.Applicative
import Control.Error
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Text (Text)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Lobster.Error
import Lobster.JSON

import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Lobster.AST    as A
import qualified Lobster.Policy as P
import qualified Lobster.Domain as D

die :: String -> IO a
die err = do
  hPutStrLn stderr err
  exitFailure

parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    []     -> die "usage: lobster-json FILENAME"
    (x:[]) -> return x

conf :: AP.Config
conf = AP.defConfig
  { AP.confIndent  = 2
  , AP.confCompare = AP.keyOrder
                       [ "name", "class", "args", "ports", "connections", "subdomains"
                       , "left", "right", "connection"
                       ]
  }

handleError :: String -> Error -> IO ()
handleError filename err = do
  forM_ (errorMessages err) $ \s ->
    hPutStrLn stderr $ filename ++ ": " ++ s
  exitFailure

main :: IO ()
main = do
  filename <- parseArgs
  eitherT (handleError filename) return $ do
    policy <- P.parsePolicyFile filename
    (es, domain) <- hoistEither $ P.interpretPolicy policy
    liftIO $ BS.putStrLn (AP.encodePretty' conf domain)

