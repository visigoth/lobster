import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "lobster-core" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "can run tests" $
      2 `compare` 2 @?= EQ
  ]
