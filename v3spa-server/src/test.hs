{-# LANGUAGE OverloadedStrings #-}

import CoreSyn (showLobster)
import Data.Monoid ((<>))
import M4ToLobster (toLobsterModule)
import SCD.M4.ModuleFiles (ModuleSource(..), addPolicyModule, readPolicyModuleSource)

importModule :: String -> FilePath -> IO ()
importModule name path = do
  iface <- readFile (path <> ".if")
  impl  <- readFile (path <> ".te")
  fctxt <- readFile (path <> ".fc")
  let modSrc = ModuleSource { moduleSourceName           = name
                            , moduleSourceInterface      = iface
                            , moduleSourceImplementation = impl
                            , moduleSourceFileContexts   = fctxt
                            }
  case readPolicyModuleSource modSrc of
    Left err -> putStrLn "error: " >> putStrLn (show err)
    Right policyModule -> 
      case toLobsterModule [] policyModule of
        Right lobster -> putStrLn (showLobster lobster)
        Left err      -> putStrLn "error" >> putStrLn (show err)

main :: IO ()
main = importModule "apache" "/home/jesse/projects/v3spa-lobster/v3spa-server/data/refpolicy/minimal/policy/modules/contrib/apache"
