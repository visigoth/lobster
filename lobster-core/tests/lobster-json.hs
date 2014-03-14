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
  hPutStrLn stderr "usage: lobster-json FILENAME"
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
  file <- parseArgs
  mod  <- eitherT (die file) return (readPolicy file)
  LBS.putStrLn (AP.encodePretty mod)

