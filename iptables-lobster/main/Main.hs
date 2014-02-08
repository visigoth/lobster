import Control.Applicative

import IptablesToLobster

import SCD.Lobster.Gen.CoreSyn.Output (showLobster)

main = do
  s <- getContents
  case toLobster s of
    Left e -> putStrLn "ERROR" >> print e
    Right lsr -> putStrLn (showLobster lsr)
