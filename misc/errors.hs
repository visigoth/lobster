import Control.Error hiding (tryIO)
import Control.Exception (IOException)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)

import qualified Control.Error as E

-- Quick demonstration of the awesomeness that is the "errors"
-- package.  The idea is to always use "Either" for pure functions
-- that can return an error, and "EitherT" for monadic ones.
--
-- The errors package makes it easy to combine them as needed,
-- as well as catch "Control.Exception" exceptions and convert
-- them to our "Error" data type.
--
-- Note that the actual logic here is bogus, the point is the
-- error handling...

-- | Errors thrown by the functions in this module.
data Error = ParseError String
           | SemanticError String
           | IOError IOException
  deriving Show

-- A pure function that can return an error.
parsePolicy :: String -> Either Error Int
parsePolicy [] = throwE (ParseError "empty policy")
parsePolicy s
  | length s < 5 = throwE (SemanticError "policy too short")
  | otherwise    = Right 42

-- We can also write pure functions in monadic style as well
-- using the Either monad:
parsePolicy' :: String -> Either Error Int
parsePolicy' s = do
  when (null s) $
    throwE (ParseError "empty policy")
  when (length s < 5) $
    throwE (SemanticError "policy too short")
  return 42

-- Execute an I/O action, catching I/O exceptions and returning
-- them as an "IOError" in our custom error type.
tryIO :: MonadIO m => IO a -> EitherT Error m a
tryIO = fmapLT IOError . E.tryIO

-- When we need effects from another Monad, use EitherT.  For
-- example, to do I/O:
readPolicy :: FilePath -> EitherT Error IO Int
readPolicy filename = do
  -- Catch IO exceptions and lift them into our Error type:
  result <- tryIO $ readFile filename
  -- We can combine the "Either Error" and "EitherT Error IO"
  -- monads using "hoistEither":
  hoistEither (parsePolicy result)

-- This should execute successfully.
test1 :: IO (Either Error Int)
test1 = runEitherT (readPolicy "/etc/passwd")

-- This should fail with an I/O error.
test2 :: IO (Either Error Int)
test2 = runEitherT (readPolicy "/doesnt/exist")

-- This should fail with a "policy too short" error.
test3 :: IO (Either Error Int)
test3 = runEitherT (readPolicy "/dev/null")

