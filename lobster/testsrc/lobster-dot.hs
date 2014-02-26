--
-- lobster-dot.hs --- Export a Lobster file in Graphviz .dot format.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--
module Main where

import System.Console.GetOpt
  (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt, usageInfo)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Lobster.Dot (dotDomainFile, simpleDotDomainFile)

die :: String -> IO a
die err = do
  hPutStrLn stderr err
  exitFailure

data Options = Options
  { simpleMode :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { simpleMode = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "s" ["simple"] (NoArg (\o -> o{ simpleMode = True })) "Hide ports for domains"
  ]

printUsage :: IO ()
printUsage = do
  p <- getProgName
  let header = "Usage:\n  " ++ p ++ " [options] <input lobster file>\nOptions:"
  putStrLn (usageInfo header options)

reportErrors :: [String] -> IO ()
reportErrors errs = do
  p <- getProgName
  putStrLn (p ++ ": " ++ concat errs)

exitErrors :: [String] -> IO a
exitErrors errs = do
  reportErrors errs
  printUsage
  exitFailure

checkOpt :: [OptDescr (a -> a)] -> a -> [String] -> IO (a,[String])
checkOpt os d args =
  case getOpt Permute os args of
    (f, r, [])   -> return (foldl (flip id) d f, r)
    (_, _, errs) -> exitErrors errs

checkOpt_ :: [String] -> IO (Options, FilePath)
checkOpt_ args = do
  (opts, fns) <- checkOpt options defaultOptions args
  case fns of
    [x] -> return (opts, x)
    [] -> exitErrors ["missing input file"]
    _ -> exitErrors ["illegal arguments"]

main :: IO ()
main = do
  args <- getArgs
  (opts, filename) <- checkOpt_ args
  let f = if simpleMode opts then simpleDotDomainFile else dotDomainFile
  dot <- f filename
  putStrLn dot
