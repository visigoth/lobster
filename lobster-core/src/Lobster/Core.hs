{-# LANGUAGE OverloadedStrings #-}
--
-- Core.hs --- Top-level interface to Lobster core.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Lobster.Core
  ( readPolicy
  , parseByteString
  , spanErrorMessage

  , module Lobster.Core.Lexer
  , module Lobster.Core.Parser
  , module Lobster.Core.AST
  , module Lobster.Core.Error
  , module Lobster.Core.Eval
  , module Lobster.Core.Pretty
  , module Lobster.Core.JSON
  ) where

import Control.Error
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Data.Text (Text, pack)

import Lobster.Core.Lexer
import Lobster.Core.Parser
import Lobster.Core.AST
import Lobster.Core.Error
import Lobster.Core.Eval
import Lobster.Core.Pretty
import Lobster.Core.JSON

import qualified Data.ByteString.Lazy as LBS

-- | Parse a bytestring and return a policy.
parseByteString :: LBS.ByteString -> Either (Error Span) (Policy Span)
parseByteString s = runAlex s parsePolicy

-- | Parse and evaluate a Lobster file.
readPolicy :: FilePath -> EitherT (Error Span) IO (Module Span)
readPolicy path = do
  contents  <- liftIO $ LBS.readFile path
  hoistEither $ do
    policy <- parseByteString contents
    evalPolicy policy

-- | Return an error message with source location information.
spanErrorMessage :: Error Span -> Text
spanErrorMessage err = lineT <> ":" <> colT <> ": " <> msg
  where
    Span (line, col) _ = errorLabel err
    lineT = pack $ show $ line
    colT  = pack $ show $ col
    msg   = errorMessage err
