{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import Control.Applicative ((<$>))
import Control.Error (EitherT, failWith, hoistEither, runEitherT)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import M4ToLobster (toLobsterModule)
import M4ToLobster.Error (Error)
import SCD.M4.ModuleFiles (ModuleSource(..), readPolicyModuleSource)
import SCD.M4.Syntax (PolicyModule(..))

import qualified CoreSyn              as L
import qualified Data.ByteString.Lazy as LBS
import qualified Lobster.Core         as C


----------------------------------------------------------------------
-- Helpers

refPolicyPath :: FilePath
refPolicyPath = "../v3spa-server/data/refpolicy/minimal"

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

getPolicyModule :: String -> FilePath -> IO PolicyModule
getPolicyModule name path = do
  source <- getModuleSource name path
  case readPolicyModuleSource source of
    Left msg           -> error msg
    Right policyModule -> return policyModule

compileToLobster :: PolicyModule -> Either String LBS.ByteString
compileToLobster policyModule = _Left %~ show $ do
  decls <- toLobsterModule [] policyModule
  return $ L.showLobsterBS decls

readLobsterPolicy :: LBS.ByteString -> Either String (C.Module C.Span)
readLobsterPolicy source = stringifyError $ C.readPolicyBS source
  where
    stringifyError = _Left %~ (unpack . C.spanErrorMessage)

reportFailure :: EitherT String IO a -> IO ()
reportFailure steps = do
  result <- runEitherT steps
  case result of
    Left msg -> assertFailure msg
    Right _  -> return ()


----------------------------------------------------------------------
-- Fixtures

apacheM4Source :: EitherT String IO ModuleSource
apacheM4Source = liftIO $ getModuleSource "apache" (refPolicyPath <> "/policy/modules/contrib/apache")

apacheM4Policy :: EitherT String IO PolicyModule
apacheM4Policy = do
  source <- apacheM4Source
  hoistEither $ readPolicyModuleSource source

apacheLobsterPolicy :: EitherT String IO (C.Module C.Span)
apacheLobsterPolicy = do
  policy  <- apacheM4Policy
  lobster <- hoistEither $ compileToLobster policy
  hoistEither $ readLobsterPolicy lobster

----------------------------------------------------------------------
-- Tests

emitRoleAnnotationTest :: TestTree
emitRoleAnnotationTest = testCase "emits 'Roles' annotation" $ reportFailure $ do
  policy <- apacheLobsterPolicy
  domain <- failWith "Could not find domain `apache::httpd_helper_t`" $
            C.pathDomain policy "apache::httpd_helper_t"
  let annot  = domain ^. C.domainAnnotation
  exprs  <- failWith "'Roles' annotation was not found" $
            C.lookupAnnotation "Roles" annot
  return ()

tests :: TestTree
tests = testGroup "genLobster"
  [ testGroup "roles" [emitRoleAnnotationTest]
  ]


----------------------------------------------------------------------
-- Main

main :: IO ()
main = defaultMain tests
