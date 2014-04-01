{-# LANGUAGE OverloadedStrings #-}
--
-- lobster-json.hs --- Convert a Lobster file to JSON.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

import Control.Error
import Control.Lens hiding ((.=))
import Control.Monad (unless)
import Data.Aeson
import Data.Monoid
import Data.Text (Text, pack)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Lobster.Core

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Graph.Inductive       as G
import qualified Data.Aeson.Encode.Pretty   as AP
import qualified Data.Text.IO               as TIO

----------------------------------------------------------------------
-- Option Processing

data Options = Options
  { optionDepth :: Maybe Int
  , optionPaths :: [Text]
  , optionIds   :: [DomainId]
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optionDepth = Nothing
  , optionPaths = []
  , optionIds   = []
  }

-- | Return true if no predicate options were supplied.
isNoDomainPred :: Options -> Bool
isNoDomainPred (Options Nothing [] []) = True
isNoDomainPred _ = False

options :: [OptDescr (Endo Options)]
options =
  [ Option ['d'] ["depth"] (ReqArg (Endo . opt_depth) "N")
    "maximum subdomain depth"
  , Option ['p'] ["path"] (ReqArg (Endo . opt_path) "PATH")
    "domain paths to expand"
  , Option ['i'] ["id"] (ReqArg (Endo . opt_id) "ID")
    "domain ids to expand"
  ]
  where
    opt_depth x opts = opts { optionDepth = Just (read x) }
    opt_path  x opts = opts { optionPaths = pack x:optionPaths opts }
    opt_id    x opts = opts { optionIds = DomainId (read x):optionIds opts }

usageHeader :: String
usageHeader =
  "Usage: lobster-json [OPTIONS] FILENAME\n" ++
  "Convert a Lobster source file to JSON.\n"

usage :: String -> IO a
usage s = do
  unless (null s) $
    hPutStrLn stderr s
  hPutStrLn stderr $ usageInfo usageHeader options
  exitFailure

parseArgs :: IO (Options, FilePath)
parseArgs = do
  args <- getArgs
  case getOpt Permute options args of
    (f, (file:[]), []) -> return (appEndo (mconcat f) defaultOptions, file)
    (_, _, []) -> usage ""
    (_, _, errs) -> usage (concat errs)

die :: FilePath -> Error Span -> IO a
die file err = do
  let msg = pack file <> ":" <> spanErrorMessage err
  TIO.hPutStrLn stderr msg
  exitFailure

makeJSON :: Module Span -> Value
makeJSON mod =
  object [ "version" .= (4 :: Int)
         , "result"  .= toJSON mod
         , "errors"  .= ([] :: [()])
         ]

maybeM_ :: Monad m => (a -> m ()) -> Maybe a -> m ()
maybeM_ f x = maybe (return ()) f x

optionsPred :: Options -> Module l -> DomainPred l
optionsPred opts m
  | isNoDomainPred opts = noDomainPred
  | otherwise =
    runDomainPredBuilder $ do
      maybeM_ (addPred . maxDepth m) (optionDepth opts)
      mapM_ (addPred . isDomainPath) (optionPaths opts)
      mapM_ (addPred . isDomainId) (optionIds opts)

main :: IO ()
main = do
  (opts, file) <- parseArgs
  mod <- eitherT (die file) return (readPolicy file)
  let p = optionsPred opts mod
  LBS.putStrLn (AP.encodePretty (moduleJSON mod p))
