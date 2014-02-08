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
                   , expectFail "unimplemented"
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

expectFail :: FilePath -> Test
expectFail file = testCase file $ do
  s <- unsafeParseIptables <$> readFile (file <.> "iptables")
  case toLobster s of
    Left _  -> return ()
    Right _ -> assertFailure "expected failure"

unsafeParseIptables :: String -> Iptables
unsafeParseIptables s = either (error . show) id (parseIptables s)
