module EvalTest
  ( unitTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens
import qualified Data.Graph.Inductive as G
import Lobster.Core

import Fixtures (withModule)

unitTests :: TestTree
unitTests = testGroup "Eval tests" $
  [ testCase "cross-module reference" $ withModule "modules.lsr" testCrossModuleReference
  ]

testCrossModuleReference :: Module Span -> Assertion
testCrossModuleReference m =
  let graph = moduleGraph m
  in putStrLn (show (G.edges (graph ^. moduleGraphGraph)))
