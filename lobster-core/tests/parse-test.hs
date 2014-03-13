--
-- parse-test.hs --- Test the Lobster parser.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

import System.Environment
import System.Exit
import System.IO
import Text.Show.Pretty (ppShow)

import Lobster.Core

import qualified Data.ByteString.Lazy as LBS

usage :: IO a
usage = do
  hPutStrLn stderr "usage: parse-test FILENAME"
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
  let (Policy _ policy) = parsePolicy toks
  -- remove source location to reduce size of shown ast
  let stmts = map (fmap $ const ()) policy
  putStrLn $ ppShow stmts

