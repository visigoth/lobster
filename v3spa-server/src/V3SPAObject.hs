{-# LANGUAGE OverloadedStrings #-}
module V3SPAObject where

import Data.Aeson.Types
import Lobster.JSON()
import qualified Lobster.Policy as P
import Lobster.Error (ErrorLoc(..))

import qualified Version as V

data V3SPAObject
  = V3SPAObject
    { errors       :: [(ErrorLoc, String)]
    , checkResults :: [Either String String]
    , domain       :: Maybe P.Domain
    , version      :: Int
    }

emptyVO :: V3SPAObject
emptyVO
  = V3SPAObject
    { errors       = []
    , checkResults = []
    , domain       = Nothing
    , version      = V.version
    }

instance ToJSON V3SPAObject where
  toJSON vo =
    object $ [ "errors"       .= toJSONMessages (errors vo)
             , "checkResults" .= toJSONMessages' (checkResults vo)
             , "version"      .= (version vo)
             ] ++ case domain vo of
                    Just dom -> [ "domain"      .= toJSON dom ]
                    Nothing -> []

toJSONMessages :: [(ErrorLoc, String)] -> Value
toJSONMessages = toJSON . map toJSONMessage
  where
    toJSONMessage (ErrorLoc file l c, m) =
      object [ "filename" .= toJSON file
             , "message"  .= toJSON m
             , "line"     .= toJSON l
             , "column"   .= toJSON c
             ]

toJSONMessages' :: [Either String String] -> Value
toJSONMessages' = toJSON . map toJSONMessage
  where toJSONMessage (Left m) = object [ "type" .= String "failure"
                                        , "message" .= toJSON m
                                        ]
        toJSONMessage (Right m) = object [ "type" .= String "success"
                                         , "message" .= toJSON m
                                         ]
