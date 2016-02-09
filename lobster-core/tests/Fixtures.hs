module Fixtures where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (unpack)
import Test.Tasty.HUnit

import Lobster.Core

exampleDir :: FilePath
exampleDir = "tests/examples/"

getFixture :: FilePath -> IO LBS.ByteString
getFixture path = LBS.readFile (exampleDir ++ path)

withPolicy :: FilePath -> (Policy Span -> Assertion) -> Assertion
withPolicy path f = do
  source <- getFixture path
  case parseByteString source of
    Left err     -> assertFailure (unpack (errorMessage err))
    Right policy -> f policy

