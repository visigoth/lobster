module ParseTest
  ( unitTests
  ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (unpack)
import Test.Tasty
import Test.Tasty.HUnit

import Lobster.Core

type Fixtures = [(FilePath, LBS.ByteString)]

unitTests :: Fixtures -> Fixtures -> TestTree
unitTests valid invalid = testGroup "Parser tests" $
  fmap (uncurry testValid) valid ++ fmap (uncurry testInvalid) invalid

testValid :: FilePath -> LBS.ByteString -> TestTree
testValid path contents = testCase (path ++ " is syntactically valid") $
  case parseByteString contents of
    Left err -> assertFailure (unpack (errorMessage err))
    Right _ -> assertSuccess

testInvalid :: FilePath -> LBS.ByteString -> TestTree
testInvalid path contents = testCase (path ++ " is not syntactically valid") $
  case parseByteString contents of
    Left _  -> assertSuccess
    Right _ -> assertFailure ("expected parse to fail")

assertSuccess :: Assertion
assertSuccess = return ()
