{-# LANGUAGE OverloadedStrings #-}
--
-- lobster-json.hs --- Export a Lobster file in JSON format.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text (Text)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Lobster.Monad (runP)
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

main :: IO ()
main = do
  filename <- parseArgs
  policy   <- P.parsePolicyFile filename
  let (es, domain) = P.interpretPolicy policy
  BS.putStrLn (AP.encodePretty' conf domain)

