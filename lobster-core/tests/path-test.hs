{-# LANGUAGE OverloadedStrings #-}
--
-- path-test.hs
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

import Control.Applicative ((<$>))
import Control.Error (runEitherT)
import Control.Lens
import Control.Monad (unless)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Text (Text)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.PrettyPrint.Mainland

import Lobster.Core

import qualified Data.Foldable as F
import qualified Data.Set      as S
import qualified Data.Text     as T
import qualified Data.Text.IO  as T

----------------------------------------------------------------------
-- Option Processing

data Options = Options
  { optionLimit       :: Maybe Int
  , optionDirection   :: GTDirection
  , optionPerms       :: Maybe (S.Set Perm)
  , optionTransPerms  :: S.Set Perm
  } deriving (Eq, Ord, Show)

defaultOptions :: Options
defaultOptions = Options
  { optionLimit       = Nothing
  , optionDirection   = GTForward
  , optionPerms       = Nothing
  , optionTransPerms  = S.empty
  }

parsePerms :: Text -> S.Set Perm
parsePerms t = S.fromList (catMaybes (map parsePerm xs))
  where
    xs = T.splitOn "," t

options :: [OptDescr (Endo Options)]
options =
  [ Option ['l'] ["limit"] (ReqArg (Endo . opt_limit) "N")
    "maximum number of leaves to return"
  , Option ['d'] ["direction"] (ReqArg (Endo . opt_direction) "DIR")
    "traversal direction, 'forward' or 'backward'"
  , Option ['p'] ["perms"] (ReqArg (Endo . opt_perms) "PERM1,PERM2...")
    "filter starting connections on permission list"
  , Option ['t'] ["trans-perms"] (ReqArg (Endo . opt_trans_perms) "PERM1,PERM2...")
    "filter transitive connections on permission list"
  ]
    where
      opt_limit x opts =
        opts { optionLimit = Just (read x) }
      opt_direction x opts
        | x == "forward"  = opts { optionDirection = GTForward }
        | x == "backward" = opts { optionDirection = GTBackward }
        | otherwise       = error "invalid direction"
      opt_perms x opts =
        opts { optionPerms = Just $ parsePerms (T.pack x) }
      opt_trans_perms x opts =
        opts { optionTransPerms = parsePerms (T.pack x) }

usageHeader :: String
usageHeader =
  "Usage: path-test [OPTIONS] FILENAME START_DOMAIN\n" ++
  "Perform a path query on a Lobster module.\n"

usage :: String -> IO a
usage s = do
  unless (null s) $
    hPutStrLn stderr s
  hPutStrLn stderr (usageInfo usageHeader options)
  exitFailure

parseArgs :: IO (Options, FilePath, String)
parseArgs = do
  args <- getArgs
  case getOpt Permute options args of
    (f, (file:dom:[]), []) ->
      return (appEndo (mconcat f) defaultOptions, file, dom)
    (_, _, [])   -> usage ""
    (_, _, errs) -> usage (concat errs)

----------------------------------------------------------------------
-- Main Program

main :: IO ()
main = do
  (opts, file, d) <- parseArgs
  print opts
  result          <- runEitherT $ readPolicy file
  case result of
    Left err -> error (show err)
    Right m  -> do
      let mdom = pathDomain m (T.pack d)
      case mdom of
        Just dom -> pathQuery m dom opts
        Nothing  -> do hPutStrLn stderr ("no such domain: " ++ d)
                       exitFailure

ppConn :: Module l -> GConn -> Text
ppConn m gc =
  let lPort = m ^. idPort (gc ^. gconnLeft) in
  let rPort = m ^. idPort (gc ^. gconnRight) in
  let connId = T.pack $ show $ getConnectionId $ gc ^. gconnId in
  view portPath lPort <> " -- " <> view portPath rPort
                      <> " (" <> connId <> ")"

ppPermAnn :: [Exp l] -> Text
ppPermAnn [ExpString (LitString _ c), ExpString (LitString _ p)] = c <> "." <> p
ppPermAnn _ = "INVALID_PERM"

ppPerms :: Module l -> GConn -> Text
ppPerms m gc =
  let conn  = m ^. idConnection (gc ^. gconnId) in
  let anns = lookupAnnotations "Perm" (conn ^. connectionAnnotation) in
  case anns of
    [] -> ""
    _  -> T.intercalate " " (anns ^.. folded . to ppPermAnn)

pathQuery :: Eq l => Module l -> Domain l -> Options -> IO ()
pathQuery m dom opts = do
  case lookupAnnotation "Type" (dom ^. domainAnnotation) of
    Just _  -> return ()
    Nothing -> do hPutStrLn stderr "domain is not a type"
                  exitFailure
  let mg = moduleGraph m
  let gr = mg ^. moduleGraphGraph
  let n  = mg ^?! moduleGraphDomainMap . ix (dom ^. domainId)
  let limit = optionLimit opts
  let dir   = optionDirection opts
  let perms = optionPerms opts
  let tperms = optionTransPerms opts
  -- let edgeP = optEdgeP opts
  let (PathSet ps, full) = getPaths m dir perms tperms 10 limit gr n
  unless full (T.putStrLn "partial results:\n")
  iforMOf_ ifolded ps $ \domId paths -> do
    let d = m ^. idDomain domId
    let domIdT = T.pack $ show $ getDomainId $ d ^. domainId
    T.putStrLn $ d ^. domainPath <> " (" <> domIdT <> "):"
    F.forM_ paths $ \path -> do
      T.putStrLn "  via path:"
      F.forM_ path $ \node -> do
        let conn = node ^. gtnodeConn
        let seg  = node ^. gtnodeSeg
        T.putStrLn ("    " <> (T.pack $ show seg) <> " " <> ppConn m conn <> " ")
        let perms = ppPerms m conn
        unless (T.null perms) $
          T.putStrLn ("     {" <> perms <> "}")
      case path of
        [] -> return ()
        _  -> do
          let lastNode = last path
          case lastNode ^. gtnodeCond of
            Just e  -> do
              putStrLn $ "  condition: " <> (pretty 1000 (ppr e))
              T.putStrLn (smt e)
            Nothing -> return ()
      T.putStrLn ""

