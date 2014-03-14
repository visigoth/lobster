--
-- eval-test.hs --- Test the Lobster evaluator.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

import Control.Error (runEitherT)
import System.Environment
import System.Exit
import System.IO

import Lobster.Core

import qualified Data.Graph.Inductive as G

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

main :: IO ()
main = do
  file   <- parseArgs
  result <- runEitherT $ readPolicy file
  case result of
    Left err -> error (show err)
    Right m  -> putStrLn (G.graphviz' (labelledGraph m))

