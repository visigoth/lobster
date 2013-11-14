--
-- lobster-dot.hs --- Export a Lobster file in Graphviz .dot format.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Lobster.Dot (dotDomainFile)

die :: String -> IO a
die err = do
  hPutStrLn stderr err
  exitFailure

parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    []     -> die "usage: lobster-dot FILENAME"
    (x:[]) -> return x

main :: IO ()
main = do
  filename <- parseArgs
  dot <- dotDomainFile filename
  putStrLn dot
