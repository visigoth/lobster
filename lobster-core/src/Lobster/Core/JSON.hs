{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
--
-- JSON.hs --- Exporting Lobster to JSON format.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

-- | This module only exports 'ToJSON' instances for Lobster
-- module types.
module Lobster.Core.JSON () where

import Control.Lens hiding ((.=))
import Data.Aeson as A
import Data.Text (Text)
import Text.PrettyPrint.Mainland (pretty, ppr)

import Lobster.Core.Lexer (Loc(..), Span(..))
import Lobster.Core.Error
import Lobster.Core.Eval
import Lobster.Core.Pretty()

import qualified Data.Graph.Inductive       as G
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Lobster.Core.AST           as A

getDomKey :: Module l -> DomainId -> Text
-- to use qualified names as keys:
-- getDomKey m domId = (m ^?! moduleDomains . ix domId) ^. domainPath
getDomKey _ (DomainId x) = T.pack $ show x

getPortKey :: Module l -> PortId -> Text
-- to use qualified names as keys:
-- getPortKey m portId = (m ^?! modulePorts . ix portId) ^. portPath
getPortKey _ (PortId x) = T.pack $ show x

instance ToJSON Loc where
  toJSON NoLoc        = Null
  toJSON (Loc (l, c)) = object ["line" .= l, "col" .= c]

instance ToJSON Span where
  toJSON (Span start end) =
    object
      [ "start" .= toJSON start
      , "end"   .= toJSON end
      ]

domainJSON :: Module Span -> DomainId -> A.Value
domainJSON m domId =
  let dom = m ^?! moduleDomains . ix domId in
    object
      [ "name"              .= (dom ^. domainName)
      , "path"              .= (dom ^. domainPath)
      , "class"             .= (dom ^. domainClassName)
      , "subdomains"        .= subdomainsJSON m dom
      , "ports"             .= portsJSON m dom
      , "classAnnotations"  .= (dom ^. domainClassAnnotation)
      , "domainAnnotations" .= (dom ^. domainAnnotation)
      , "srcloc"            .= toJSON (A.label dom)
      ]

portJSON :: Module Span -> PortId -> A.Value
portJSON m portId =
  let port = m ^?! modulePorts . ix portId in
    object
      [ "name"          .= (port ^. portName)
      , "path"          .= (port ^. portPath)
      , "annotations"   .= (port ^. portAnnotation)
      , "srcloc"        .= toJSON (A.label port)
      , "domain"        .= (port ^. portDomain . to (getDomKey m))
      ]

connectionJSON :: Module Span -> Connection Span -> A.Value
connectionJSON m c =
  object
    [ "left"        .= (c ^. connectionLeft . to (getPortKey m))
    , "right"       .= (c ^. connectionRight . to (getPortKey m))
    , "level"       .= (c ^. connectionLevel)
    , "connection"  .= (c ^. connectionType)
    , "annotations" .= (c ^. connectionAnnotation)
    , "srcloc"      .= (c ^. connectionLabel)
    ]


subdomainsJSON :: Module Span -> Domain Span -> A.Value
subdomainsJSON m dom =
  toJSON $ S.map (getDomKey m) $ dom ^. domainSubdomains

portsJSON :: Module Span -> Domain Span -> A.Value
portsJSON m dom =
  toJSON $ S.map (getPortKey m) $ dom ^. domainPorts

instance ToJSON ConnLevel where
  toJSON ConnLevelPeer   = "peer"
  toJSON ConnLevelParent = "parent"
  toJSON ConnLevelChild  = "child"

instance ToJSON A.ConnType where
  toJSON A.ConnLeftToRight   = "left-to-right"
  toJSON A.ConnRightToLeft   = "right-to-left"
  toJSON A.ConnBidirectional = "bidirectional"
  toJSON A.ConnNeutral       = "neutral"

-- | Convert an unevaluated expression to JSON.  This is only
-- used for annotations.
instance ToJSON (A.Exp l) where
  toJSON (A.ExpInt (A.LitInteger _ x)) = toJSON x
  toJSON (A.ExpString (A.LitString _ x)) = toJSON x
  -- XXX for now, just convert to strings
  toJSON e = toJSON (pretty 0 (ppr e))
  -- toJSON _ = error "unsupported annotation expression"

instance ToJSON (A.Annotation l) where
  toJSON (A.Annotation xs) = toJSON (map go xs)
    where
      go (A.TypeName _ name, args) =
        object
          [ "name"  .= name
          , "args"  .= args
          ]

connections :: Module Span -> [Connection Span]
connections m = map go (G.labEdges $ m ^. moduleGraph)
  where
    go (_, _, conn) = conn

instance ToJSON (Module Span) where
  toJSON m =
    object
      [ "domains"     .= toJSON domains
      , "ports"       .= toJSON ports
      , "connections" .= toJSON (map (connectionJSON m) (connections m))
      , "root"        .= toJSON (getDomKey m (m ^. moduleRootDomain))
      ]
    where
      domains = M.fromList . map goD . M.toList $ m ^. moduleDomains
      ports   = M.fromList . map goP . M.toList $ m ^. modulePorts
      goD (domId, _)   = (getDomKey m domId, domainJSON m domId)
      goP (portId, _)  = (getPortKey m portId, portJSON m portId)

instance ToJSON (Error Span) where
  toJSON e =
    object
      [ "filename"    .= ("<unknown>" :: Text)  -- FIXME
      , "message"     .= errorMessage e
      , "srcloc"      .= maybe Null toJSON (errorLabel e)
      ]
