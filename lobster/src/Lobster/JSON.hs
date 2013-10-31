{-# LANGUAGE OverloadedStrings #-}
--
-- JSON.hs --- Exporting Lobster domains in JSON format.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Lobster.JSON where

import Control.Applicative ()
import Control.Monad ()
import Data.Aeson
import Data.Text (Text)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Lobster.Monad (runP)

import qualified Data.Map as M

import qualified Lobster.Abs    as A
import qualified Lobster.Policy as P
import qualified Lobster.Domain as D

instance ToJSON D.DomainId where
  toJSON (D.DomainId x) = toJSON x

portIdName :: A.PortId -> String
portIdName (A.PortId (A.LIdent x)) = x

flowIdName :: A.FlowId -> String
flowIdName (A.FlowId (A.LIdent x)) = x

dirJSON :: Text -> Value
dirJSON s = toJSON s
-- dirJSON s = object ["direction" .= s]

instance ToJSON A.Direction where
  toJSON A.InputDirection         = dirJSON "in"
  toJSON A.OutputDirection        = dirJSON "out"
  toJSON A.BidirectionalDirection = dirJSON "inout"

connJSON :: Text -> Value
connJSON s = toJSON s

instance ToJSON A.Connection where
  toJSON A.BidirectionalConnection = connJSON "bidirectional"
  toJSON A.LeftToRightConnection   = connJSON "left-to-right"
  toJSON A.RightToLeftConnection   = connJSON "right-to-left"
  toJSON A.NeutralConnection       = connJSON "neutral"

instance ToJSON P.ContextClass where
  toJSON x = toJSON (P.prettyPrintContextClass x)

posJSON :: Text -> Value
posJSON s = toJSON s
--posJSON s = object ["position" .= s]

instance ToJSON A.Position where
  toJSON A.SubjectPosition = posJSON "subject"
  toJSON A.ObjectPosition  = posJSON "object"

instance ToJSON a => ToJSON (D.PortTypeValue a) where
  toJSON (D.Direction x)       = toJSON x
  toJSON (D.Value x)           = toJSON x
  toJSON D.Inconsistent        = Null

instance ToJSON P.PortTypeValue where
  toJSON (P.TypePortTypeValue x)      = toJSON x
  toJSON (P.PositionPortTypeValue x)  = toJSON x

instance ToJSON a => ToJSON (D.PortType a) where
  toJSON (D.PortType x) = toJSON (M.mapKeys flowIdName x)

instance ToJSON D.DomainPort where
  toJSON (D.DomainPort (Just domId) p) =
    object [ "domain" .= toJSON domId
           , "port"   .= portIdName p
           ]
  toJSON (D.DomainPort Nothing p) =
    object [ "port" .= portIdName p ]

instance ToJSON P.Value where
  toJSON (P.IntValue x)                   = toJSON x
  toJSON (P.StringValue x)                = toJSON x
  toJSON (P.DirectionValue x)             = toJSON x
  toJSON (P.PositionValue x)              = toJSON x
  toJSON (P.ClassValue x)                 = toJSON x
  toJSON (P.PortTypeValue (P.PortType x)) = toJSON x
  toJSON (P.DomainValue x)                = object ["domain" .= toJSON x]
  toJSON (P.DomainPortValue x)            = toJSON x

instance ToJSON P.Domain where
  toJSON d =
    object
      [ "name"        .= P.nameDomain d
      , "subdomains"  .= toJSON (M.fromList subdomains)
      , "class"       .= P.prettyPrintContextClass ctx
      , "args"        .= map toJSON args
      , "ports"       .= toJSON (M.mapKeys portIdName (ports d))
      , "connections" .= conns d
      ]
    where
      goDom (D.DomainId domId) dom xs = (show domId, dom) : xs
      subdomains  = P.foldSubDomain goDom [] d
      (ctx, args) = P.provenanceDomain d
      ports (P.Domain d') = D.ports d'
      goConn p1 c p2 xs = obj : xs
        where
          obj = object
                  [ "left"       .= toJSON p1
                  , "right"      .= toJSON p2
                  , "connection" .= toJSON c
                  ]
      conns (P.Domain d') = D.foldConnectionsDomain goConn [] d'
