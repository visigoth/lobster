import Control.Applicative

import IptablesToLobster

import SCD.Lobster.Gen.CoreSyn.Output (showLobster)

main = do
  s <- getContents
  case toLobster $ unsafeParseIptables s of
    Left e -> putStrLn "ERROR" >> print e
    Right lsr -> putStrLn (showLobster lsr)

unsafeParseIptables s = either (error . show) id (parseIptables s)

example = unsafeParseIptables <$> readFile "example.iptables"
ftp = unsafeParseIptables <$> readFile "ftp.iptables"
