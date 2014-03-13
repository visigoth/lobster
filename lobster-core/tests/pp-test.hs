--
-- pp-test.hs --- Test the Lobster pretty printer.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

import System.Environment
import System.Exit
import System.IO
import Text.PrettyPrint.Mainland

import Lobster.Core

import qualified Data.ByteString.Lazy as LBS

usage :: IO a
usage = do
  hPutStrLn stderr "usage: pp-test FILENAME"
  exitFailure

parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    a:_ -> return a
    []  -> usage

main :: IO ()
main = do
  file      <- parseArgs
  contents  <- LBS.readFile file
  let toks   = alexScanTokens contents
  let policy = parsePolicy toks
  putDoc (ppr policy <> line)

