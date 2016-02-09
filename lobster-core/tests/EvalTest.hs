module EvalTest
  ( unitTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

-- import Lobster.Core

import Fixtures (withPolicy)

unitTests :: TestTree
unitTests = testGroup "Eval tests" $
  [ testModules
  ]

testModules :: TestTree
testModules = testCase "making cross-module references" $
  withPolicy "modules.lsr" $ \policy -> do
    assertSuccess

assertSuccess :: Assertion
assertSuccess = return ()
