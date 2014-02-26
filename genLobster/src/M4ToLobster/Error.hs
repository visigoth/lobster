{-# OPTIONS -Wall -Werror #-}
module M4ToLobster.Error where

import Control.Error

-- | Placeholder error type.  This should be split out into
-- a different module and have multiple constructors for each
-- type of error.
data Error = Error String
  deriving (Eq, Ord, Show)

-- | Handle an error in the I/O monad when running the command line
-- program.
handleError :: Error -> IO a
handleError (Error s) = error s

-- | Run an "EitherT Error IO a" action, handling errors with
-- "handleError", otherwise returning the "a" in the IO monad.
--
-- This is a convenience wrapper around "runEitherT"
-- for use in the command line tool.
runErr :: EitherT Error IO a -> IO a
runErr = eitherT handleError return

-- | Run an IO action, catching exceptions and returning them
-- as an "Error".  Use this instead of "liftIO".
runIO :: IO a -> EitherT Error IO a
runIO f = fmapLT (Error . show) (tryIO f)

