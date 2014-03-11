{-# LANGUAGE OverloadedStrings #-}
--
-- Main.hs
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

import Control.Applicative ((<$>))
import Control.Error
import Control.Exception
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))

import Lobster.Lexer (alexNoPos)
import Lobster.Error
import Lobster.JSON
import SCD.Lobster.Gen.CoreSyn.Output (showLobster)
import SCD.M4.ModuleFiles
import Snap

import V3SPAObject

import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.Encoding  as E
import qualified Data.Text.Lazy.IO        as TIO
import qualified Data.Aeson.Encode.Pretty as AP

import qualified Lobster.Policy           as P
import qualified SCD.M4.Syntax            as M4
import qualified SCD.Lobster.Gen.CoreSyn  as L
import qualified Version                  as V
import qualified IptablesToLobster        as I
import qualified M4ToLobster              as M

----------------------------------------------------------------------
-- Snap Utilities

-- FIXME: Error handling in here is all fucked up...  We need an
-- EitherT over "Snap" I think.

-- | Snap monad extended with a "Reader" for configuration data.
type V3Snap a = ReaderT Options Snap a

-- | Read the request body as a string.
readBodyString :: V3Snap String
readBodyString = T.unpack . E.decodeUtf8 <$> readRequestBody 10000000

-- | Encode a result as JSON and write it to the response.
sendVO :: V3SPAObject -> V3Snap ()
sendVO vo = do
  writeLBS (AP.encodePretty' conf vo)
  writeLBS "\r\n"

-- | Return an error object's source location and message string.
buildError :: Error -> (ErrorLoc, String)
buildError (LocError loc err) = (loc, errorMessage err)
buildError err = (unknownLoc, errorMessage err)

-- | Send an error response as JSON from an error object.
sendError :: Error -> V3Snap a
sendError err = do
  sendVO $ emptyVO { errors = [buildError err] }
  r <- getResponse
  finishWith r

-- | Run an action that may fail in the "V3Snap" monad,
-- catching errors and returning them as a JSON response.
handleErr :: Err a -> (a -> V3Snap ()) -> V3Snap ()
handleErr (Left err) _ = sendError err
handleErr (Right x) f  = f x

----------------------------------------------------------------------
-- Request Handlers

-- | "GET /version" --- request web service version
handleVersion :: V3Snap ()
handleVersion = method GET $ do
  modifyResponse $ setContentType "application/json"
  let obj = object [ "version" .= V.version ]
  writeLBS (AP.encodePretty obj)
  writeLBS "\r\n"

-- | Pretty JSON configuration for parsed Lobster.
conf :: AP.Config
conf = AP.defConfig
  { AP.confIndent  = 2
  , AP.confCompare = AP.keyOrder
                       [ "name", "class", "args", "ports"
                       , "connections", "subdomains"
                       , "left", "right", "connection"
                       ]
  }

-- | Parse Lobster from a request body string.
parseLobster :: String -> Err ([Either String String], P.Domain)
parseLobster body = P.parsePolicy body >>= P.toDomain

-- | "POST /parse" --- parse Lobster to JSON
handleParse :: V3Snap ()
handleParse = method POST $ do
  modifyResponse $ setContentType "application/json"
  body <- readBodyString
  handleErr (parseLobster body) $ \(checks, dom) -> do
    sendVO $ emptyVO { checkResults = checks
                     , domain = Just dom
                     }

-- | Parse IPtables from a request body string.
importIptables :: String -> Err [L.Decl]
importIptables = fmapL (MiscError . show) . I.toLobster

-- | "POST /import/iptables" --- import IPTables to Lobster
handleImportIptables :: V3Snap ()
handleImportIptables = method POST $ do
  modifyResponse $ setContentType "application/json"
  body <- readBodyString
  handleErr (importIptables body) $ \lsr -> do
    let obj = object [ "version" .= V.version
                     , "result"  .= showLobster lsr
                     , "errors"  .= ([] :: [()])
                     ]
    writeLBS (AP.encodePretty obj)
    writeLBS "\r\n"

data SELinuxImportRequest = SELinuxImportRequest
  { seReqRefpolicy :: String
  , seReqModules   :: [ModuleSource]
  } deriving Show

instance FromJSON SELinuxImportRequest where
  parseJSON (Object v) =
    SELinuxImportRequest <$> v .: "refpolicy"
                         <*> v .: "modules"
  parseJSON _ = mzero

-- XXX kind of lousy orphan instance here
instance FromJSON ModuleSource where
  parseJSON (Object v) =
    ModuleSource <$> v .: "name"
                 <*> v .: "if"
                 <*> v .: "te"
                 <*> v .: "fc"
  parseJSON _ = mzero

importModule :: M4.Policy -> ModuleSource -> V3Snap M4.Policy
importModule p modSrc =
  case readPolicyModuleSource modSrc of
    Left err  -> do sendError $ MiscError err
                    return p
    Right mod -> return $ addPolicyModule mod p

-- | "POST /import/selinux" --- import SELinux to Lobster
handleImportSELinux :: V3Snap ()
handleImportSELinux = method POST $ do
  modifyResponse $ setContentType "application/json"
  body <- readRequestBody 10000000
  let mreq = decode body :: Maybe SELinuxImportRequest
  case mreq of
    Nothing -> sendError $ MiscError "malformed request"
    Just req -> do
      dir <- refPolicyDir (seReqRefpolicy req)
      policy0 <- liftIO $ readPolicy Nothing dir
      policy1 <- foldM importModule policy0 (seReqModules req)
      case M.toLobster M.Mode2 policy1 of
        Left err  -> sendError $ MiscError (show err)
        Right lsr -> do
          let obj = object [ "version" .= V.version
                           , "result"  .= showLobster lsr
                           , "errors"  .= ([] :: [()])
                           ]
          writeLBS (AP.encodePretty obj)
          writeLBS "\r\n"

----------------------------------------------------------------------
-- Routing

-- | Routing information for the web service.
site :: V3Snap ()
site = route
  [ ("/parse",           handleParse)
  , ("/version",         handleVersion)
  , ("/import/iptables", handleImportIptables)
  , ("/import/selinux",  handleImportSELinux)
  ]

----------------------------------------------------------------------
-- Option Processing and Main

-- | Web service options (from command line).
data Options = Options
  { optionDataDir :: Maybe FilePath
  } deriving Show

-- | Default options.
defaultOptions :: Options
defaultOptions = Options
  { optionDataDir = Nothing
  }

-- | Return the directory that contains reference policy versions.
refPolicyBaseDir :: V3Snap FilePath
refPolicyBaseDir = do
  dataDir <- fromMaybe "." <$> asks optionDataDir
  return $ dataDir </> "refpolicy"

-- | Return the directory for a reference policy by name.
refPolicyDir :: String -> V3Snap FilePath
refPolicyDir name = (</> name) <$> refPolicyBaseDir

main :: IO ()
main = do
  let options = defaultOptions
  quickHttpServe (runReaderT site options)
