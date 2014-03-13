--
-- Lexer.x --- Lexical analysis for Lobster.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

{
module Lobster.Core.Lexer
  ( -- * Source Spans
    Span(..)
  , unionSpan

    -- * Token Types
  , Keyword(..)
  , Operator(..)
  , ConnOperator(..)
  , TokenType(..)
  , Token(..)

    -- * Lexer
  , alexScanTokens
  ) where

import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text             as T
}

%wrapper "posn-bytestring"

$upper        = [A-Z]
$lower        = [a-z]
$identLetter  = [A-Za-z0-9_]
$digit        = [0-9]
$u            = [\x00-\xFF]

@string   = ([. # [\" \\ \n]] | (\\ (\" | \\ | \' | n | t)))
@lident   = $lower $identLetter*
@uident   = $upper $identLetter*

:-

-- Skip comments and whitespace.
"//" [.]* ;
"/*" ([$u # \*] | \* [$u # \/])* ("*")+ "/" ;
$white+ ;

$digit+             { tok tokInt }
\" @string* \"      { tok_ TokString }

----------------------------------------------------------------------
-- Keywords and Identifiers

"assert"            { kw KwAssert }
"bidirectional"     { kw KwBidirectional }
"class"             { kw KwClass }
"domain"            { kw KwDomain }
"exists"            { kw KwExists }
"input"             { kw KwInput }
"never"             { kw KwNever }
"object"            { kw KwObject }
"output"            { kw KwOutput }
"port"              { kw KwPort }
"subject"           { kw KwSubject }
"this"              { kw KwThis }

@lident             { tok_ TokLIdent }
@uident             { tok_ TokUIdent }

----------------------------------------------------------------------
-- Operators

"("                 { op OpLParen }
")"                 { op OpRParen }
"{"                 { op OpLBrace }
"}"                 { op OpRBrace }
"["                 { op OpLBracket }
"]"                 { op OpRBracket }
";"                 { op OpSemi }
","                 { op OpComma }
"="                 { op OpEquals }
"->"                { op OpRArrow }
":"                 { op OpColon }
"::"                { op OpDoubleColon }
"."                 { op OpPeriod }
"*"                 { op OpStar }
".*"                { op OpDotStar }

"-->"               { connOp OpConnLeftToRight }
"<--"               { connOp OpConnRightToLeft }
"<-->"              { connOp OpConnBidirectional }
"--"                { connOp OpConnNeutral }

{
-- | A Lobster language keyword.
data Keyword
  = KwAssert
  | KwBidirectional
  | KwClass
  | KwDomain
  | KwExists
  | KwInput
  | KwNever
  | KwObject
  | KwOutput
  | KwPort
  | KwSubject
  | KwThis
  deriving (Eq, Ord, Show)

-- | A reserved operator.
data Operator
  = OpLParen
  | OpRParen
  | OpLBrace
  | OpRBrace
  | OpLBracket
  | OpRBracket
  | OpSemi
  | OpComma
  | OpEquals
  | OpRArrow
  | OpColon
  | OpDoubleColon
  | OpPeriod
  | OpStar
  | OpDotStar
  deriving (Eq, Ord, Show)

-- | A connection operator.
data ConnOperator
  = OpConnLeftToRight
  | OpConnRightToLeft
  | OpConnBidirectional
  | OpConnNeutral
  deriving (Eq, Ord, Show)

-- | A source span (probably should be in its own module).
data Span = Span
  { spanStart :: (Int, Int)
  , spanEnd   :: (Int, Int)
  } deriving (Eq, Ord, Show)

-- | Create a source span from a token position and text.
mkTokSpan :: Integral a => AlexPosn -> a -> Span
mkTokSpan (AlexPn _ line col) len = Span start end
  where
    start = (line, col)
    end   = (line, col + fromIntegral len)

-- | Take the union of two source ranges.
unionSpan :: Span -> Span -> Span
unionSpan s1 s2
  | s1 < s2   = Span (spanStart s1) (spanEnd s2)
  | otherwise = Span (spanStart s2) (spanEnd s1)

-- | Data common to all token types.
data Token = Token
  { tokSpan   :: Span
  , tokText   :: T.Text
  , tokType   :: TokenType
  } deriving Show

-- | Data for each type of token.
data TokenType
  = TokKeyword      Keyword
  | TokOperator     Operator
  | TokConnOperator ConnOperator
  | TokInteger      Integer
  | TokString
  | TokUIdent
  | TokLIdent
  deriving Show

-- | Token type builder for an integer literal.
tokInt :: T.Text -> TokenType
tokInt t =
  case decimal t of
    Right (x, _) -> (TokInteger x)
    Left err     -> error err   -- XXX better error

-- | Build a token from a token type constructor function.
tok :: (T.Text -> TokenType) -> AlexPosn -> LBS.ByteString -> Token
tok f pos bs = Token span text ty
  where
    span = mkTokSpan pos (T.length text)
    text = decodeUtf8 (LBS.toStrict bs)
    ty   = f text

-- | Build a token from a nullary token type constructor.
tok_ :: TokenType -> AlexPosn -> LBS.ByteString -> Token
tok_ = tok . const

-- | Build a keyword token.
kw :: Keyword -> AlexPosn -> LBS.ByteString -> Token
kw k = tok_ (TokKeyword k)
-- k pos bs = TokKeyword (mkTokSpan pos (LBS.length bs)) k

-- | Build an operator token.
op :: Operator -> AlexPosn -> LBS.ByteString -> Token
op o = tok_ (TokOperator o)

-- | Build a connection operator token.
connOp :: ConnOperator -> AlexPosn -> LBS.ByteString -> Token
connOp o = tok_ (TokConnOperator o)
}

-- vim: set ft=alex ts=2 et:
