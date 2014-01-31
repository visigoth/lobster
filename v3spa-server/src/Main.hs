{-# LANGUAGE OverloadedStrings #-}
--
-- Main.hs
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

import Control.Applicative ((<$>))
import Control.Exception
import Data.Monoid ((<>))

import Lobster.Lexer (alexNoPos)
import Lobster.Monad (runP)
import Lobster.JSON
import Snap

import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.Encoding  as E
import qualified Data.Text.Lazy.IO        as TIO

import qualified Data.Aeson.Encode.Pretty as AP

import qualified Lobster.Policy           as P

import V3SPAObject

conf :: AP.Config
conf = AP.defConfig
  { AP.confIndent  = 2
  , AP.confCompare = AP.keyOrder
                       [ "name", "class", "args", "ports"
                       , "connections", "subdomains"
                       , "left", "right", "connection"
                       ]
  }

errorHandler :: ErrorCall -> IO (Either String a)
errorHandler (ErrorCall s) = return (Left s)

-- | Interpret a policy, returning the domain or an error
-- string.  We have to go to some trouble here because the
-- interpreter just bombs with "error" if there is an error.
interpret :: P.Policy a -> IO (Either String P.Domain)
interpret p = catch (return . Right . snd $ P.interpretPolicy p) errorHandler

sendVO :: V3SPAObject -> Snap ()
sendVO vo = do
  writeLBS (AP.encodePretty' conf vo)
  writeLBS "\r\n"

handleParse :: Snap ()
handleParse = method POST $ do
  body <- (T.unpack . E.decodeUtf8) <$> readRequestBody 10000000
  let policy = P.parsePolicy body
  modifyResponse $ setContentType "application/json"
  case policy of
    Left err -> sendVO $ emptyVO { errors = [err] }
    Right p  -> do
      let result = runP (P.toDomain p)
      case result of
        Left err -> sendVO $ emptyVO { errors = [(alexNoPos, err)] }
        Right (checks, dom) ->
          sendVO $ emptyVO { checkResults = checks
                           , domain = Just dom
                           }

site :: Snap ()
site = route [("/parse", handleParse)]

main :: IO ()
main = quickHttpServe site
