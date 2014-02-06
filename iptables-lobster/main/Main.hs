import Control.Applicative

import IptablesToLobster

import SCD.Lobster.Gen.CoreSyn.Output (showLobster)

main = do
  s <- getContents
  putStrLn . showLobster . toLobster $ unsafeParseIptables s

unsafeParseIptables s = either (error . show) id (parseIptables s)

example = unsafeParseIptables <$> readFile "example.iptables"
ftp = unsafeParseIptables <$> readFile "ftp.iptables"
