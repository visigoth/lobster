module Lobster.Common where

import Control.Monad.Trans (liftIO)
import Control.Error (hoistEither)

import System.Console.GetOpt(getOpt, usageInfo,ArgOrder(..), OptDescr(..), ArgDescr(..))
import System.Environment(getProgName, getArgs)
import System.Exit(exitFailure)

import Lobster.Error
import Lobster.Policy

import qualified Lobster.Lexer as Lex

data Options = Options
  { includeFiles :: [String]
  , outputFile :: String
  } deriving (Eq, Read, Show, Ord)

-- FIXME: these two functions feel especially bogus right now...
parseAndInterpretPolicyFiles_ :: Options -> [FilePath] -> ErrT IO Domain
parseAndInterpretPolicyFiles_ options fns = do
  (errs,d) <- parseAndInterpretPolicyFiles options fns
  if null errs
    then return d
    else error $ "ERROR: symbion errors in Lobster policy file(s) " ++ ":\n" ++
           unlines errs

parseAndInterpretPolicyFiles :: Options -> [FilePath] -> ErrT IO ([String],Domain)
parseAndInterpretPolicyFiles options fns = do
  let files = includeFiles options ++ fns
  policy <- parsePolicyFiles files
  (eexs,dom) <- hoistEither $ interpretPolicy policy
  sequence_ [ liftIO $ putStrLn x | Right x <- eexs ]
  return ([ e | Left e <- eexs ],dom)

processOptions :: IO (Options,[String])
processOptions =
    do args <- getArgs
       case getOpt RequireOrder programOptions args of
         (fs,work,[]) -> return (foldl (flip id) defaultOptions fs, work)
         (_,_,errs) -> do p <- getProgName
                          putStrLn (p ++ ": " ++ concat errs)
                          usage

defaultOptions :: Options
defaultOptions = Options
  { includeFiles = []
  , outputFile = "module"
  }

programOptions :: [OptDescr (Options -> Options)]
programOptions =
    [ Option ['I'] ["include"] (ReqArg addIncludeOptions "include.lsr")
       "Include a lobster file"
    , Option ['o'] ["module"] (ReqArg setOutputOptions "module")
       "Set the name of the output module"
    ]

addIncludeOptions :: String -> Options -> Options
addIncludeOptions file options =
    options {includeFiles = includeFiles options ++ [file]}

setOutputOptions :: String -> Options -> Options
setOutputOptions file options = options {outputFile = file}

usage :: IO a
usage =
    do p <- getProgName
       let header =
             "Usage:\n" ++ p ++ " [-I include.lsr] [-o module] input.lsr ..."
       putStrLn $ usageInfo header programOptions
       exitFailure

parsePolicyFiles :: [String] -> ErrT IO (Policy Lex.Posn)
parsePolicyFiles filenames =
    case filenames of
      [] -> return empty
      filename : filenames' ->
        do policy <- parsePolicyFile filename
           policy' <- parsePolicyFiles filenames'
           return (append policy policy')

