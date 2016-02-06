module LexTest
  ( unitTests
  ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (unpack)
import Test.Tasty
import Test.Tasty.HUnit

import Lobster.Core

type Fixtures = [(FilePath, LBS.ByteString)]

unitTests :: Fixtures -> Fixtures -> TestTree
unitTests valid invalid = testGroup "Lexer tests" $
  fmap (uncurry testValid) valid

testValid :: FilePath -> LBS.ByteString -> TestTree
testValid path input = testCase (path ++ " contains only valid tokens") $
  case scanner input of
    Left e     -> assertFailure (unpack (errorMessage e))
    Right toks -> length toks > 0 @? "lexer produced a token stream"

scanner :: LBS.ByteString -> Either (Error Span) [Token]
scanner s = runAlex s $ do
  let loop xs = do
        tok <- alexMonadScan
        case tok of
          Token _ _ TokEOF -> return xs
          _ -> loop $ tok : xs
  loop []
