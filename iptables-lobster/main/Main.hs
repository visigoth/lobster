import Control.Applicative

import Iptables.Parser
import Iptables.Print
import Iptables.Types

import qualified SCD.Lobster.Gen.CoreSyn as L
import SCD.Lobster.Gen.CoreSyn.Output (showLobster)

main = do
  s <- getContents
  putStr $ echo s

echo :: String -> String
echo = printIptables . unsafeParseIptables

unsafeParseIptables s = either (error . show) id (parseIptables s)

toLobster :: Iptables -> [L.Decl]
toLobster ipts = preamble ++ [mkHost (L.Name "host") []]

preamble = [ruleC, ruleD, userC, userD, destC, destD]
  where ruleC = L.newComment "An iptables rule corresponding to a single rule in a chain"
        ruleD = L.newClass (L.Name "Rule") [L.Name "condition"] ruleDs
        ruleDs = [ L.newComment "Incoming packet"
                 , L.newPort (L.Name "in")
                 , L.newComment "Outgoing packet when condition is true"
                 , L.newPort (L.Name "match")
                 , L.newComment "Outgoing packet when condition is false"
                 , L.newPort (L.Name "fail")
                 ]
        userC = L.newComment "Abstract representation of userspace"
        userD = L.newClass (L.Name "UserSpace") [] userDs
        userDs = [ L.newComment "Packets entering userspace"
                 , L.newPort (L.Name "accept")
                 , L.newComment "Packets leaving userspace"
                 , L.newPort (L.Name "out")
                 ]
        destC = L.newComment "Abstract representation of a packet's destination outside of the current policy (eg DROP, REJECT, FORWARD)"
        destD = L.newClass (L.Name "Destination") [] [L.newPort (L.Name "in")]

-- | Create the abstract host with the given declarations added after
-- the standard set of @incoming@, @outgoing@, @forward@, @reject@,
-- @drop@, and @user@.
mkHost :: L.Name -> [L.Decl] -> L.Decl
mkHost name decls = L.newClass name [] (builtins ++ decls)
  where builtins = [ L.newComment "All incoming interfaces"
                   , L.newPort (L.Name "incoming")
                   , L.newComment "All outgoing interfaces"
                   , L.newPort (L.Name "outgoing")
                   , L.newComment "Packets destined for forwarding to another host"
                   , L.newPort (L.Name "forward")
                   , L.newComment "Rejected packets flow here"
                   , L.newDomain (L.Name "reject") (L.Name "Destination") []
                   , L.newComment "Dropped packets flow here"
                   , L.newDomain (L.Name "drop") (L.Name "Destination") []
                   , L.newComment "This host's userspace"
                   , L.newDomain (L.Name "user") (L.Name "UserSpace") []
                   ]

example = unsafeParseIptables <$> readFile "example.iptables"
ftp = unsafeParseIptables <$> readFile "ftp.iptables"
