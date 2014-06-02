{-# LANGUAGE OverloadedStrings #-}
--
-- path-test.hs
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

import Control.Error (runEitherT)
import Control.Lens
import Control.Monad (unless)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Tree
import System.Environment
import System.Exit
import System.IO
import Text.PrettyPrint.Mainland

import Lobster.Core

import qualified Data.Foldable as F
import qualified Data.Set      as S
import qualified Data.Text     as T
import qualified Data.Text.IO  as T

usage :: IO a
usage = do
  hPutStrLn stderr "usage: path-test FILENAME START_DOMAIN [LIMIT]"
  exitFailure

parseArgs :: IO (FilePath, String, Maybe Int)
parseArgs = do
  args <- getArgs
  case args of
    a:b:[]   -> return (a, b, Nothing)
    a:b:c:[] -> return (a, b, Just (read c))
    _        -> usage

main :: IO ()
main = do
  (file, d, limit) <- parseArgs
  result           <- runEitherT $ readPolicy file
  case result of
    Left err -> error (show err)
    Right m  -> do
      let mdom = pathDomain m (T.pack d)
      case mdom of
        Just dom -> pathQuery m dom limit
        Nothing  -> do hPutStrLn stderr ("no such domain: " ++ d)
                       exitFailure

gconnRightDomain :: Module l -> GConn -> DomainId
gconnRightDomain m conn = dom ^. domainId
  where
    port = m ^. idPort (conn ^. gconnRight)
    dom  = m ^. idDomain (port ^. portDomain)

leaves :: Module l -> [Tree GConn] -> S.Set DomainId
leaves _ [] = S.empty
leaves m (Node x [] : ys) =
  S.union (S.singleton $ gconnRightDomain m x) (leaves m ys)
leaves m (Node _ xs : ys) =
  S.union (leaves m xs) (leaves m ys)

ppConn :: Module l -> GConn -> Text
ppConn m gc =
  let conn  = m ^. idConnection (gc ^. gconnId) in
  let lPort = m ^. idPort (gc ^. gconnLeft) in
  let rPort = m ^. idPort (gc ^. gconnRight) in
  view portPath lPort <> " -- " <> view portPath rPort

ppPerms :: Module l -> GConn -> Text
ppPerms m gc =
  let conn  = m ^. idConnection (gc ^. gconnId) in
  let anns = lookupAnnotations "Perm" (conn ^. connectionAnnotation) in
  case anns of
    [] -> ""
    _  -> T.intercalate " " (anns ^.. folded . folded . _ExpString . to getLitString)

pathQuery :: Eq l => Module l -> Domain l -> Maybe Int -> IO ()
pathQuery m dom limit = do
  case lookupAnnotation "Type" (dom ^. domainAnnotation) of
    Just _  -> return ()
    Nothing -> do hPutStrLn stderr "domain is not a type"
                  exitFailure
  let mg = moduleGraph m
  let gr = mg ^. moduleGraphGraph
  let n  = mg ^?! moduleGraphDomainMap . ix (dom ^. domainId)
  let (ts, full) =
        case limit of
          Nothing -> (getPaths m forwardEdges 10 gr n, True)
          Just l  -> getPathsWithLimit m forwardEdges 10 l gr n
  unless full (T.putStrLn "partial results:\n")
  iforMOf_ ifolded (getPathSet m ts) $ \domId paths -> do
    let d = m ^. idDomain domId
    case lookupAnnotation "Type" (d ^. domainAnnotation) of
      Just _  -> do
        T.putStrLn $ d ^. domainPath <> ":"
        F.forM_ paths $ \path -> do
          T.putStrLn "  via path:"
          F.forM_ path $ \node -> do
            let conn = node ^. pathNodeConn
            T.putStrLn ("    " <> ppConn m conn <> " ")
            let perms = ppPerms m conn
            unless (T.null perms) $
              T.putStrLn ("     {" <> perms <> "}")
            -- putStr (show (getConnectionId connId) <> " ")
          case path of
            [] -> return ()
            _  -> do
              let lastNode = last path
              case lastNode ^. pathNodeExp of
                Just e  -> putStrLn $ "  condition: " <> (pretty 1000 (ppr e))
                Nothing -> return ()
          T.putStrLn ""
      Nothing -> return ()

