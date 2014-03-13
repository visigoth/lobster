--
-- lex-test.hs --- Test the Lobster lexer.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

import System.Environment
import System.Exit
import System.IO

import Lobster.Core

import qualified Data.ByteString.Lazy as LBS

usage :: IO a
usage = do
  hPutStrLn stderr "usage: lex-test FILENAME"
  exitFailure

parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    a:_ -> return a
    []  -> usage

main :: IO ()
main = do
  file     <- parseArgs
  contents <- LBS.readFile file
  let toks  = alexScanTokens contents
  mapM_ print toks

