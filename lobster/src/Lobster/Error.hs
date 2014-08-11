--
-- Error.hs --- Lobster error handling.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--
-- Released under the "BSD3" license.  See the file "LICENSE"
-- for details.
--

module Lobster.Error where

import Control.Error
import Data.List (intercalate)
import Data.Maybe
import Text.Printf

import Lobster.AST as A

-- | Location of an error in a source file.
data ErrorLoc = ErrorLoc String Integer Integer
  deriving (Eq, Ord, Show)

-- | Special value for an unknown error location.
unknownLoc :: ErrorLoc
unknownLoc = ErrorLoc "unknown" (-1) (-1)

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
  | LocError ErrorLoc Error     -- nested error with source location
  deriving Show

-- | A pure computation that can fail with an error.
type Err a = Either Error a

-- | A monadic computation that can fail with an error.
type ErrT m a = EitherT Error m a

-- | Return a single string error message.
errorMessage :: Error -> String
errorMessage err = intercalate "\n" (errorMessages err)

-- | Return a list of error messages for an Error object.
errorMessages :: Error -> [String]
errorMessages err =
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
    LocError (ErrorLoc _ line col) err ->
      [printf "line %d, column %d:" line col] ++ errorMessages err

errorLocFromAnnotation :: A.AnnotationElement -> Maybe ErrorLoc
errorLocFromAnnotation ann =
  case ann of
    (A.UIdent "SourcePos",
      [ A.StringExpression filename
      , A.IntExpression line
      , A.IntExpression column
      ]) -> Just (ErrorLoc filename line column)
    _ -> Nothing

-- | Tag an error with a location if it isn't already.  This will get
-- more complex if we add other nested error types.
mkLocError :: ErrorLoc -> Error -> Error
mkLocError _ err@(LocError _ _) = err
mkLocError loc err = LocError loc err

-- Note that for now we are taking the first source position and
-- discarding the rest.
annotateErrorLoc :: Annotation -> Error -> Err a
annotateErrorLoc (Annotation ann) err =
  case mapMaybe errorLocFromAnnotation ann of
    (loc:_) -> throwE (mkLocError loc err)
    []      -> throwE err
