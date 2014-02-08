{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import System.FilePath

import Iptables.Types
import IptablesToLobster
import SCD.Lobster.Gen.CoreSyn.Output (showLobster)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual, assertFailure)

main :: IO ()
main = defaultMain [ testGold "example"
                   , testGold "ftp"
                   , testGold "emptyuserchain"
                   , expectFail (\case { EUnknownTarget _ -> True; _ -> False }) "unknowntarget"
                   ]

testGold :: FilePath -> Test
testGold file = testCase file $ do
  s <- unsafeParseIptables <$> readFile (file <.> "iptables")
  case toLobster s of
    Left e -> assertFailure (show e)
    Right lsr -> do
      -- command-line tool adds a trailing newline
      let actual = (showLobster lsr) ++ "\n"
      expected <- readFile (file <.> "lsr")
      assertEqual "" actual expected

expectFail :: (Error -> Bool) -> FilePath -> Test
expectFail errPred file = testCase file $ do
  s <- unsafeParseIptables <$> readFile (file <.> "iptables")
  case toLobster s of
    Left e | errPred e -> return ()
           | otherwise -> assertFailure ("wrong kind of error: " ++ show e)
    Right _ -> assertFailure "expected an error, but translation succeeded"

unsafeParseIptables :: String -> Iptables
unsafeParseIptables s = either (error . show) id (parseIptables s)
