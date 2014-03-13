--
-- Core.hs --- Top-level interface to Lobster core.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Lobster.Core
  ( readPolicy

  , module Lobster.Core.Lexer
  , module Lobster.Core.Parser
  , module Lobster.Core.AST
  , module Lobster.Core.Eval
  , module Lobster.Core.Pretty
  , module Lobster.Core.JSON
  ) where

import Control.Error
import Control.Monad.IO.Class

import Lobster.Core.Lexer
import Lobster.Core.Parser
import Lobster.Core.AST
import Lobster.Core.Eval
import Lobster.Core.Pretty
import Lobster.Core.JSON

import qualified Data.ByteString.Lazy as LBS

-- | Parse and evaluate a Lobster file.
readPolicy :: FilePath -> EitherT (Error Span) IO (Module Span)
readPolicy path = do
  contents  <- liftIO $ LBS.readFile path
  let toks   = alexScanTokens contents
  -- XXX need to deal with parse errors in the monad here
  let policy = parsePolicy toks
  hoistEither $ evalPolicy policy

