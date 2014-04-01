{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
--
-- JSON.hs --- Exporting Lobster to JSON format.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Lobster.Core.JSON where

import Control.Lens hiding ((.=))
import Data.Aeson as A
import Data.Monoid (mempty)
import Data.Text (Text)
import Text.PrettyPrint.Mainland (pretty, ppr)

import Lobster.Core.Lexer (Loc(..), Span(..))
import Lobster.Core.Error
import Lobster.Core.Eval
import Lobster.Core.Traverse
import Lobster.Core.Pretty()

import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Lobster.Core.AST           as A

getDomKey :: DomainId -> Text
getDomKey (DomainId x) = T.pack $ show x

getPortKey :: PortId -> Text
getPortKey (PortId x) = T.pack $ show x

instance ToJSON Loc where
  toJSON NoLoc        = Null
  toJSON (Loc (l, c)) = object ["line" .= l, "col" .= c]

instance ToJSON Span where
  toJSON (Span start end) =
    object
      [ "start" .= toJSON start
      , "end"   .= toJSON end
      ]

domainJSON :: Module l -> DomainTree Span -> A.Value
domainJSON m dt =
  let dom = dt ^. domainTreeDomain in
    object
      [ "name"              .= (dom ^. domainName)
      , "path"              .= (dom ^. domainPath)
      , "class"             .= (dom ^. domainClassName)
      , "subdomains"        .= subdomainsJSON m dom
      , "parent"            .= maybe Null (toJSON . getDomKey) (dom ^. domainParent)
      , "ports"             .= portsJSON dom
      , "classAnnotations"  .= (dom ^. domainClassAnnotation)
      , "domainAnnotations" .= (dom ^. domainAnnotation)
      , "srcloc"            .= toJSON (A.label dom)
      ]

portJSON :: Port Span -> A.Value
portJSON port =
  object
    [ "name"          .= (port ^. portName)
    , "path"          .= (port ^. portPath)
    , "annotations"   .= (port ^. portAnnotation)
    , "srcloc"        .= toJSON (A.label port)
    , "domain"        .= (port ^. portDomain . to getDomKey)
    ]

connectionJSON :: Connection Span -> A.Value
connectionJSON c =
  object
    [ "left"        .= (c ^. connectionLeft . to getPortKey)
    , "right"       .= (c ^. connectionRight . to getPortKey)
    , "level"       .= (c ^. connectionLevel)
    , "connection"  .= (c ^. connectionType)
    , "annotations" .= (c ^. connectionAnnotation)
    , "srcloc"      .= (c ^. connectionLabel)
    ]

{-
-- if we don't want to show filtered out subdomains
subdomainsJSON :: DomainTree Span -> A.Value
subdomainsJSON dt = toJSON $ dt ^.. subdomainKeys
  where
    subdomainKeys = domainTreeSubdomains . traverse . domainTreeDomain
                                         . domainId . to getDomKey
-}

subdomainJSON :: Module l -> DomainId -> (Text, Value)
subdomainJSON m domId =
  (getDomKey domId, object ["name" .= (m ^. idDomain domId . domainName)])

subdomainsJSON :: Module l -> Domain Span -> A.Value
subdomainsJSON m dom = toJSON $ M.fromList $ map (subdomainJSON m) subdomains
  where
    subdomains = dom ^.. domainSubdomains . folded

portsJSON :: Domain Span -> A.Value
portsJSON dom =
  toJSON $ S.map getPortKey $ dom ^. domainPorts

instance ToJSON ConnLevel where
  toJSON ConnLevelPeer   = "peer"
  toJSON ConnLevelParent = "parent"
  toJSON ConnLevelChild  = "child"
  toJSON _ = error "invalid conn level in JSON"

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

moduleJSON :: Module Span -> DomainPred Span -> A.Value
moduleJSON m p =
  object [ "domains"        .= toJSON domains
         , "ports"          .= toJSON ports
         , "connections"    .= toJSON (map connectionJSON conns)
         , "root"           .= toJSON (getDomKey (m ^. moduleRootDomain))
         ]
  where
    domTree  = domainTreeWith m p
    domains  = M.fromList $ map goD $ flattenDomainTree domTree
    ports    = M.fromList $ map goP $ allPorts domTree
    conns    = connectionsWith m p
    goD dt   = (getDomKey (dt ^. domainTreeDomain . domainId), domainJSON m dt)
    goP port = (getPortKey (port ^. portId), portJSON port)

instance ToJSON (Module Span) where
  toJSON m = moduleJSON m mempty

instance ToJSON (Error Span) where
  toJSON e =
    object
      [ "filename"    .= ("<unknown>" :: Text)  -- FIXME
      , "message"     .= errorMessage e
      , "srcloc"      .= maybe Null toJSON (errorLabel e)
      ]
