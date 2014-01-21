import Iptables.Parser
import Iptables.Print

main = do
  str <- getContents
  case parseIptables str of
    Left err -> error (show err)
    Right ipts -> putStr $ printIptables ipts
  return ()
