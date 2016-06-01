{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

import M4ToLobster (toLobsterModule)
import SCD.M4.ModuleFiles (ModuleSource(..), readPolicyModuleSource)
import SCD.M4.Syntax (PolicyModule(..))

import qualified CoreSyn      as L
import qualified Lobster.Core as C

refPolicyPath :: FilePath
refPolicyPath = "../v3spa-lobster/data/refpolicy/minimal"

-- Import SELinux module from refpolicy
getModuleSource :: String -> FilePath -> IO ModuleSource
getModuleSource name path = do
  iface <- readFile (path <> ".if")
  impl  <- readFile (path <> ".te")
  fctxt <- readFile (path <> ".fc")
  return ModuleSource { moduleSourceName           = name
                      , moduleSourceInterface      = iface
                      , moduleSourceImplementation = impl
                      , moduleSourceFileContexts   = fctxt
                      }

handleError :: Either e a -> a
handleError (Left msg) = error msg
handleError (Right x) = x

getPolicyModule :: String -> FilePath -> IO PolicyModule
getPolicyModule name path = do
  source <- getModuleSource name path
  case readPolicyModuleSource source of
    Left msg           -> error msg
    Right policyModule -> return policyModule

compileToLobster :: PolicyModule -> C.Policy C.Span
compileToLobster policyModule = case toLobsterModule [] policyModule of
  Left msg      -> error msg
  Right lobster -> return $ Policy C.emptySpan lobster

evalLobster :: C.Policy -> C.Module C.Span
evalLobster policy = handleError $ C.evalPolicy policy

apacheM4Source :: IO ModuleSource
apacheM4Source = getModuleSource "apache" (refPolicyPath <> "/policy/modules/contrib/apache")

apacheM4Policy :: IO PolicyModule
apacheM4Policy = apacheM4Source >>= handleError . readPolicyModuleSource

emitRoleAnnotationTest :: TestTree
emitRoleAnnotationTest = testCase "emits 'Roles' annotation" $ do
  policyModule <- apacheM4Policy
  let lsr = handleError (toLobsterModule policyModule)

tests :: TestTree
tests = testGroup "genLobster"
  [ testGroup "roles" [emitRoleAnnotationTest]
  ]

main :: IO ()
main = defaultMain tests
