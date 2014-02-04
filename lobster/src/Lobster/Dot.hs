--
-- Dot.hs --- Exporting Lobster domains in Graphviz .dot format.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Lobster.Dot where

import Data.Traversable
import Control.Applicative
import Control.Error (runEitherT, hoistEither)
import Control.Monad (ap, liftM)
import System.IO
import System.Process

import qualified Data.Map as Map

import Lobster.AST
import Lobster.Domain
import qualified Lobster.Policy as P

import qualified Text.Dot as Dot -- Andy Gill's "dotgen" package

-- FIXME: The "dotgen" package should provide these instances!!!!
instance Functor Dot.Dot where
  fmap = liftM

instance Applicative Dot.Dot where
  pure = return
  (<*>) = ap
--


type PortNodes = Map.Map PortId Dot.NodeId

dotDomain :: Domain a b -> Dot.Dot PortNodes
dotDomain dom = (fmap snd . Dot.cluster) $ do
  Dot.attribute ("label", name dom)
  env <- Map.traverseWithKey dotPortType (ports dom)
  envs <- traverse dotDomain (subDomains dom)
  _ <- traverse (dotConnection (env, envs)) (Map.assocs (connections dom))
  return env

dotPortType :: PortId -> PortType b -> Dot.Dot Dot.NodeId
dotPortType pid@(PortId (LIdent s)) _ = do
  Dot.node [("label", s), ("shape", "oval")]
  -- We ignore the PortType for now. It might make sense to eventually
  -- use a record shape to list all the components of the PortType.

dotConnection :: (PortNodes, Map.Map DomainId PortNodes)
              -> ((DomainPort, DomainPort), Connection) -> Dot.Dot ()
dotConnection (env, envs) ((dp1, dp2), c) = do
  let idOf dp =
        case domain dp of
          Nothing -> Map.lookup (port dp) env
          Just di -> Map.lookup di envs >>= Map.lookup (port dp)
  case (idOf dp1, idOf dp2) of
    (Just i1, Just i2) -> Dot.edge i1 i2 [("dir", dirConnection c)]
    _                  -> fail "invalid DomainPort"

dirConnection :: Connection -> String
dirConnection conn =
  case conn of
    NeutralConnection -> "none"
    LeftToRightConnection -> "forward"
    RightToLeftConnection -> "back"
    BidirectionalConnection -> "both"

-- | Read domain from .lsr file.
parseDomainFile :: FilePath -> IO P.Domain
parseDomainFile filename = do
  result <- runEitherT $ do
    policy <- P.parsePolicyFile filename
    (_, dom) <- hoistEither $ P.toDomain policy
    return dom
  case result of
    Left e    -> error $ "ERROR: Unable to process:\n" ++ show e
    Right dom -> return dom

-- | Read .lsr input file, return .dot code as a string.
dotDomainFile :: FilePath -> IO String
dotDomainFile filename = do
  P.Domain dom <- parseDomainFile filename
  return $ Dot.showDot (dotDomain dom)

-- | Read .lsr input file, and write .dot output file.
writeDotOfDomainFile :: FilePath -> FilePath -> IO ()
writeDotOfDomainFile infile outfile = do
  content <- dotDomainFile infile
  writeFile outfile content

-- | Read .lsr input file, and write .pdf output file.
writePdfOfDomainFile :: FilePath -> FilePath -> IO ()
writePdfOfDomainFile infile outfile = do
  content <- dotDomainFile infile
  outh <- openFile outfile WriteMode
  (Just inh, _, _, pid) <-
      createProcess (proc "dot" ["-Tpdf"])
        { std_in  = CreatePipe,
          std_out = UseHandle outh,
          std_err = Inherit }
  hPutStr inh content
  hFlush inh
  hClose inh -- done with stdin
  ex <- waitForProcess pid
  ex `seq` hClose outh
