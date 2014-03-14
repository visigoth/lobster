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

-- this might belong in the lexer
scanner :: LBS.ByteString -> Either (Error Span) [Token]
scanner s = runAlex s $ do
  let loop xs = do
        tok <- alexMonadScan
        case tok of
          Token _ _ TokEOF -> return xs
          _ -> loop $ tok : xs
  loop []

main :: IO ()
main = do
  file     <- parseArgs
  contents <- LBS.readFile file
  case scanner contents of
    Left err   -> error (show err)
    Right toks -> mapM_ print (reverse toks)
