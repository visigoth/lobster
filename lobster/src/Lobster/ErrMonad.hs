module Lobster.ErrMonad where

-- the Error monad: like Maybe type with error msgs

import Control.Monad (MonadPlus(..), liftM)

import Lobster.Lexer

data Err a = Ok a | Bad Posn String
  deriving (Show, Eq, Ord)

instance Monad Err where
  return        = Ok
  fail          = Bad alexNoPos
  Ok a    >>= f = f a
  Bad p s >>= f = Bad p s

instance Functor Err where
  fmap = liftM

instance MonadPlus Err where
  mzero = Bad alexNoPos "Err.mzero"
  mplus (Bad _ _) y = y
  mplus x         _ = x
