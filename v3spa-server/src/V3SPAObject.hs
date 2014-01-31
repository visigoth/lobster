{-# LANGUAGE OverloadedStrings #-}
module V3SPAObject where

import Data.Aeson.Types
import Lobster.JSON()
import qualified Lobster.Policy as P
import Lobster.Lexer (Posn(..))

data V3SPAObject
  = V3SPAObject
    { errors       :: [(Posn, String)]
    , checkResults :: [Either String String]
    , domain       :: Maybe P.Domain
    }

emptyVO :: V3SPAObject
emptyVO
  = V3SPAObject
    { errors       = []
    , checkResults = []
    , domain       = Nothing
    }

instance ToJSON V3SPAObject where
  toJSON vo =
    object $ [ "errors"       .= toJSONMessages (errors vo)
             , "checkResults" .= toJSONMessages' (checkResults vo)
             ] ++ case domain vo of
                    Just dom -> [ "domain"      .= toJSON dom ]
                    Nothing -> []

toJSONMessages :: [(Posn, String)] -> Value
toJSONMessages = toJSON . map toJSONMessage
  where toJSONMessage (Pn _ l c, m) = object [ "message" .= toJSON m
                                             , "line"    .= toJSON l
                                             , "column"  .= toJSON c
                                             ]

toJSONMessages' :: [Either String String] -> Value
toJSONMessages' = toJSON . map toJSONMessage
  where toJSONMessage (Left m) = object [ "type" .= String "failure"
                                        , "message" .= toJSON m
                                        ]
        toJSONMessage (Right m) = object [ "type" .= String "success"
                                         , "message" .= toJSON m
                                         ]
