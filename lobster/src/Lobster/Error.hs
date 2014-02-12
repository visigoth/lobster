--
-- Error.hs --- Lobster error handling.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Lobster.Error where

import Control.Error
import Text.Printf

-- | Errors thrown by functions in this module.
data Error
  = SyntaxError String         -- TODO: add position
  | UnboundVar String
  | UnboundType String
  | BadConnection String String String
  | SelfConnection String
  | UndefinedPort String
  | DuplicatePort String
  | UndefinedDomain String
  | UndefinedClass String
  | DuplicateClass String
  | NonEmptyClass String        -- non-empty class used as type
  | TypeMismatch String String
  | DuplicateBinding String
  | BadArgument String          -- domain or port passed as arg
  | MiscError String
  deriving Show

-- | A pure computation that can fail with an error.
type Err a = Either Error a

-- | A monadic computation that can fail with an error.
type ErrT m a = EitherT Error m a

-- | Return a pretty error message for an Error object.
errorMessage :: Error -> [String]
errorMessage err =
  case err of
    SyntaxError s ->
      ["syntax error"]
    UnboundVar s ->
      [printf "unbound variable '%s'" s]
    UnboundType s ->
      [printf "unbound type '%s'" s]
    BadConnection p1 p2 s ->
      [printf "bad connection between '%s' and '%s':" p1 p2, " " ++ s]
    SelfConnection s ->
      [printf "cannot connection port '%s' to itself" s]
    UndefinedPort s ->
      [printf "undefined port '%s'" s]
    DuplicatePort s ->
      [printf "duplicate port definition '%s'" s]
    UndefinedDomain s ->
      [printf "undefined domain '%s'" s]
    UndefinedClass s ->
      [printf "undefined class '%s'" s]
    DuplicateClass s ->
      [printf "duplicate class definition '%s'" s]
    NonEmptyClass s ->
      [printf "cannot use non-empty class '%s' as port type" s]
    TypeMismatch e t ->
      [printf "value '%s' not of expected type '%s'" e t]
    DuplicateBinding s ->
      [printf "duplicate definition '%s'" s]
    BadArgument s ->
      [printf "cannot pass value of type '%s' as domain argument" s]
    MiscError s ->
      [s]

