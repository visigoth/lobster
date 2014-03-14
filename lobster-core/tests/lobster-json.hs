{-# LANGUAGE OverloadedStrings #-}
--
-- lobster-json.hs --- Convert a Lobster file to JSON.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

import Control.Error
import Data.Monoid ((<>))
import Data.Text (pack)
import System.Environment
import System.Exit
import System.IO

import Lobster.Core

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Graph.Inductive       as G
import qualified Data.Aeson.Encode.Pretty   as AP
import qualified Data.Text.IO               as TIO

usage :: IO a
usage = do
  hPutStrLn stderr "usage: eval-test FILENAME"
  exitFailure

parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    a:_ -> return a
    []  -> usage

die :: FilePath -> Error Span -> IO a
die file err = do
  let msg = pack file <> ":" <> spanErrorMessage err
  TIO.hPutStrLn stderr msg
  exitFailure

main :: IO ()
main = do
  file   <- parseArgs
  result <- runEitherT $ readPolicy file
  case result of
    Left err  -> die file err
    Right mod -> LBS.putStrLn (AP.encodePretty mod)

