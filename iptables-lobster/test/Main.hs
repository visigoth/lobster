import Control.Applicative
import System.FilePath

import Iptables.Types
import IptablesToLobster
import SCD.Lobster.Gen.CoreSyn.Output (showLobster)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual)

main :: IO ()
main = defaultMain [ testGold "example", testGold "ftp" ]

testGold :: FilePath -> Test
testGold file = testCase file $ do
  s <- unsafeParseIptables <$> readFile (file <.> "iptables")
  let actual = showLobster . toLobster $ s
      -- command-line tool adds a trailing newline
      actual' = actual ++ "\n"
  expected <- readFile (file <.> "lsr")
  assertEqual "" actual' expected

unsafeParseIptables :: String -> Iptables
unsafeParseIptables s = either (error . show) id (parseIptables s)
