--
-- eval-test.hs --- Test the Lobster evaluator.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

import Control.Lens
import System.Environment
import System.Exit
import System.IO

import Lobster.Core

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Graph.Inductive       as G
import qualified Data.Aeson.Encode.Pretty   as AP

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

eval :: Policy Span -> IO (Module Span)
eval policy =
  case evalPolicy policy of
    Left err  -> error (show err)
    Right mod -> return mod

main :: IO ()
main = do
  file      <- parseArgs
  contents  <- LBS.readFile file
  let toks   = alexScanTokens contents
  let policy = parsePolicy toks
  mod       <- eval policy
  LBS.putStrLn (AP.encodePretty mod)

