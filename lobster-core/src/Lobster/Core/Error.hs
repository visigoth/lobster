{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
--
-- Error.hs --- Lobster error type.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

module Lobster.Core.Error
  ( Error(..)
  , errorMessage
  , errorLabel
  ) where

import Control.Error (headMay)
import Data.Foldable (Foldable)
import Data.Monoid ((<>))
import Data.Text (Text, pack)

import qualified Data.Foldable as F

-- | Errors raised by the evaluator.
data Error l
  = LexError l Text
  | ParseError l Text
  | DuplicatePort l Text
  | DuplicateClass l Text
  | DuplicateDomain l Text
  | DuplicateVar l Text
  | UndefinedClass l Text
  | UndefinedVar l Text
  | UndefinedDomain l Text
  | UndefinedPath l Text
  | UndefinedPort l Text
  | InternalConnection l Text Text
  | BadArguments l Int
  | BadPosition l Text Text Text
  | BadNegativeConn l Text Text
  | TypeError l Text
  | MiscError String
  deriving (Show, Functor, Foldable)

-- | Extract the label from an error using the derived
-- 'Foldable' instance.
errorLabel :: Error l -> Maybe l
errorLabel = headMay . F.toList

-- | Return an error message string for an error.
errorMessage :: Error l -> Text
errorMessage err =
  case err of
    LexError _ t   -> t
    ParseError _ t -> t
    MiscError t    -> pack t
    DuplicatePort _ t ->
      "duplicate port definition: '" <> t <> "'"
    DuplicateDomain _ t ->
      "duplicate domain definition: '" <> t <> "'"
    DuplicateClass _ t ->
      "duplicate class definition: '" <> t <> "'"
    DuplicateVar _ t ->
      "duplicate variable definition: '" <> t <> "'"
    UndefinedClass _ t ->
      "undefined class: '" <> t <> "'"
    UndefinedVar _ t ->
      "undefined variable: '" <> t <> "'"
    UndefinedDomain _ t ->
      "undefined domain: '" <> t <> "'"
    UndefinedPath _ t ->
      "undefined module or domain path: '" <> t <> "'"
    UndefinedPort _ t ->
      "undefined port: '" <> t <> "'"
    BadPosition _ p1 p2 t ->
      "invalid connection from '" <> p1 <> "' to '" <> p2 <> "': " <> t
    BadNegativeConn _ p1 p2 ->
      "negative connection from '" <> p1 <> "' to '" <> p2 <> "' is not internal"
    InternalConnection _ t1 t2 ->
      "invalid internal connection from '" <> t1 <> "' to '" <> t2 <> "'"
    BadArguments _ n ->
      "wrong number of arguments, expecting " <> (pack $ show n)
    TypeError _ t ->
      "type mismatch, expecting " <> t
