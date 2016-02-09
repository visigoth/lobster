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
unitTests valid _ = testGroup "Parser tests" $
  fmap (uncurry testValid) valid

testValid :: FilePath -> LBS.ByteString -> TestTree
testValid path contents = testCase (path ++ " is syntactically valid") $
  case parseByteString contents of
    Left err -> assertFailure (unpack (errorMessage err))
    Right _ -> assertSuccess

assertSuccess :: Assertion
assertSuccess = return ()
