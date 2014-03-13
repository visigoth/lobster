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

import Lobster.Core.Lexer (Span(..))
import Lobster.Core.Eval

import qualified Data.Aeson.Encode.Pretty   as AP
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Graph.Inductive       as G
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Lobster.Core.AST           as A

getDomKey :: Module l -> DomainId -> Text
-- to use qualified names as keys:
-- getDomKey mod domId = (mod ^?! moduleDomains . ix domId) ^. domainPath
getDomKey _ (DomainId x) = T.pack $ show x

getPortKey :: Module l -> PortId -> Text
-- to use qualified names as keys:
-- getPortKey mod portId = (mod ^?! modulePorts . ix portId) ^. portPath
getPortKey _ (PortId x) = T.pack $ show x

instance ToJSON Span where
  toJSON (Span (sL, sC) (eL, eC)) =
    object
      [ "start" .= object [ "line" .= sL, "col" .= sC ]
      , "end"   .= object [ "line" .= eL, "col" .= eC ]
      ]

domainJSON :: Module Span -> DomainId -> A.Value
domainJSON mod domId =
  let dom = mod ^?! moduleDomains . ix domId in
    object
      [ "name"              .= (dom ^. domainName)
      , "path"              .= (dom ^. domainPath)
      , "class"             .= (dom ^. domainClassName)
      , "subdomains"        .= subdomainsJSON mod dom
      , "ports"             .= portsJSON mod dom
      , "classAnnotations"  .= (dom ^. domainClassAnnotation)
      , "domainAnnotations" .= (dom ^. domainAnnotation)
      , "srcloc"            .= toJSON (A.label dom)
      ]

portJSON :: Module Span -> PortId -> A.Value
portJSON mod portId =
  let port = mod ^?! modulePorts . ix portId in
    object
      [ "name"          .= (port ^. portName)
      , "path"          .= (port ^. portPath)
      , "annotations"   .= (port ^. portAnnotation)
      , "srcloc"        .= toJSON (A.label port)
      ]

subdomainsJSON :: Module Span -> Domain Span -> A.Value
subdomainsJSON mod dom =
  toJSON $ S.map (getDomKey mod) $ dom ^. domainSubdomains

portsJSON :: Module Span -> Domain Span -> A.Value
portsJSON mod dom =
  toJSON $ S.map (getPortKey mod) $ dom ^. domainPorts

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
  toJSON (A.ExpInt (A.LitInteger _ x))   = toJSON x
  toJSON (A.ExpString (A.LitString _ x)) = toJSON x
  toJSON _ = error "unsupported annotation expression"

instance ToJSON (A.Annotation l) where
  toJSON (A.Annotation xs) = toJSON (map go xs)
    where
      go (A.TypeName _ name, args) =
        object
          [ "name"  .= name
          , "args"  .= args
          ]

instance ToJSON (Connection Span) where
  toJSON c =
    object
      [ "left"        .= (c ^. connectionLeft . to show)
      , "right"       .= (c ^. connectionRight . to show)
      , "level"       .= (c ^. connectionLevel)
      , "connection"  .= (c ^. connectionType)
      , "annotations" .= (c ^. connectionAnnotation)
      , "srcloc"      .= (c ^. connectionLabel)
      ]

connections :: Module Span -> [Connection Span]
connections mod = map go (G.labEdges $ mod ^. moduleGraph)
  where
    go (_, _, conn) = conn

instance ToJSON (Module Span) where
  toJSON mod =
    object
      [ "domains"     .= toJSON domains
      , "ports"       .= toJSON ports
      , "connections" .= toJSON (connections mod)
      , "root"        .= toJSON (getDomKey mod (mod ^. moduleRootDomain))
      ]
    where
      domains = M.fromList . map goD . M.toList $ mod ^. moduleDomains
      ports   = M.fromList . map goP . M.toList $ mod ^. modulePorts
      goD (domId, dom) = (getDomKey mod domId, domainJSON mod domId)
      goP (portId, p)  = (getPortKey mod portId, portJSON mod portId)

