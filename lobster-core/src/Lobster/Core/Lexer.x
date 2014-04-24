--
-- Lexer.x --- Lexical analysis for Lobster.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

{
-- the alex wrapper code generates a lot of warnings...
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Lobster.Core.Lexer
  ( -- * Source Spans
    Loc(..)
  , Span(..)
  , unionSpan
  , emptySpan

    -- * Token Types
  , Keyword(..)
  , Operator(..)
  , ConnOperator(..)
  , ExpOperator(..)
  , TokenType(..)
  , Token(..)

    -- * Lexer
  , Alex()
  , alexMonadScan
  , alexError
  , runAlex
  ) where

import Data.Char (chr)
import Data.Monoid (Monoid(..))
import Data.Word (Word8)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)

import Lobster.Core.Error

import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text             as T

import qualified Data.ByteString.Lazy     as ByteString
import qualified Data.ByteString.Internal as ByteString (w2c)
}

-- this is broken in 3.1.3... we include the wrapper
-- so we can modify it to return an Error instead of a String
-- on failure.
--
-- %wrapper "monad-bytestring"

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
"explicit"          { kw KwExplicit }
"input"             { kw KwInput }
"never"             { kw KwNever }
"object"            { kw KwObject }
"output"            { kw KwOutput }
"port"              { kw KwPort }
"subject"           { kw KwSubject }
"this"              { kw KwThis }

"true"              { tok_ (TokBool True) }
"false"             { tok_ (TokBool False) }

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
"-/-"               { connOp OpConnNegative }

"&&"                { expOp ExpOpAnd }
"||"                { expOp ExpOpOr }
"=="                { expOp ExpOpEqual }
"!="                { expOp ExpOpNotEqual }
"!"                 { expOp ExpOpNot }

{
-- | A Lobster language keyword.
data Keyword
  = KwAssert
  | KwBidirectional
  | KwClass
  | KwDomain
  | KwExists
  | KwExplicit
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

-- | An operator that can appear in an expression.
data ExpOperator
  = ExpOpAnd
  | ExpOpOr
  | ExpOpEqual
  | ExpOpNotEqual
  | ExpOpNot
  deriving (Eq, Ord, Show)

-- | A connection operator.
data ConnOperator
  = OpConnLeftToRight
  | OpConnRightToLeft
  | OpConnBidirectional
  | OpConnNeutral
  | OpConnNegative
  deriving (Eq, Ord, Show)

-- | A source location.
data Loc = Loc (Int, Int)     -- ^ known line and column
         | NoLoc              -- ^ unknown loc
  deriving (Eq, Ord, Show)

-- | A source span.
data Span = Span
  { spanStart :: Loc
  , spanEnd   :: Loc
  } deriving (Eq, Ord, Show)

-- | An empty span.
emptySpan :: Span
emptySpan = Span NoLoc NoLoc

-- | Create a source span from a token position and text.
mkTokSpan :: Integral a => AlexPosn -> a -> Span
mkTokSpan (AlexPn _ line col) len = Span start end
  where
    start = Loc (line, col)
    end   = Loc (line, col + fromIntegral len)

-- | Take the union of two source ranges.
unionSpan :: Span -> Span -> Span
unionSpan (Span NoLoc end1) (Span start2 end2) =
  Span start2 (max end1 end2)
unionSpan (Span start1 NoLoc) (Span start2 end2) =
  Span (min start1 start2) end2
unionSpan (Span start1 end1) (Span NoLoc end2) =
  Span start1 (max end1 end2)
unionSpan (Span start1 end1) (Span start2 NoLoc) =
  Span (min start1 start2) end1
unionSpan (Span start1 end1) (Span start2 end2) =
  Span (min start1 start2) (max end1 end2)

-- | Data common to all token types.
data Token = Token
  { tokSpan   :: Span
  , tokText   :: T.Text
  , tokType   :: TokenType
  } deriving Show

instance Monoid Span where
  mempty  = emptySpan
  mappend = unionSpan

-- | Data for each type of token.
data TokenType
  = TokKeyword      Keyword
  | TokOperator     Operator
  | TokConnOperator ConnOperator
  | TokExpOperator  ExpOperator
  | TokInteger      Integer
  | TokString
  | TokBool         Bool
  | TokUIdent
  | TokLIdent
  | TokEOF
  deriving Show

alexEOF :: Alex Token
alexEOF = do
  return $ Token emptySpan (T.empty) TokEOF

-- | Token type builder for an integer literal.
tokInt :: T.Text -> Alex TokenType
tokInt t =
  case decimal t of
    Right (x, _) -> return $ TokInteger x
    Left _ -> do
      (pos,_,_) <- alexGetInput
      let span = mkTokSpan pos (T.length t)
      alexError $ LexError span (T.pack "invalid integer literal")

-- | Build a token from a token type constructor function.
tok :: (T.Text -> Alex TokenType) -> AlexPosn -> LBS.ByteString -> Alex Token
tok f pos bs = do
  x <- f text
  return $ Token span text x
  where
    span = mkTokSpan pos (T.length text)
    text = decodeUtf8 (LBS.toStrict bs)

-- | Build a token from a nullary token type constructor.
tok_ :: TokenType -> AlexPosn -> LBS.ByteString -> Alex Token
tok_ x = tok (const (return x))

-- | Build a keyword token.
kw :: Keyword -> AlexPosn -> LBS.ByteString -> Alex Token
kw k = tok_ (TokKeyword k)

-- | Build an operator token.
op :: Operator -> AlexPosn -> LBS.ByteString -> Alex Token
op o = tok_ (TokOperator o)

-- | Build a connection operator token.
connOp :: ConnOperator -> AlexPosn -> LBS.ByteString -> Alex Token
connOp o = tok_ (TokConnOperator o)

-- | Build an expression operator token.
expOp :: ExpOperator -> AlexPosn -> LBS.ByteString -> Alex Token
expOp o = tok_ (TokExpOperator o)

-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  ByteString.ByteString)        -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes i = i   -- no pending bytes when lexing bytestrings

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,_,cs) | ByteString.null cs = Nothing
                     | otherwise = let b   = ByteString.head cs
                                       cs' = ByteString.tail cs
                                       c   = ByteString.w2c b
                                       p'  = alexMove p c
                                    in p' `seq` cs' `seq` Just (b, (p', c, cs'))

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)

data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: ByteString.ByteString,      -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_scd :: !Int        -- the current startcode
    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: ByteString.ByteString -> Alex a -> Either (Error Span) a
runAlex input (Alex f) 
   = case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input,       
                        alex_chr = '\n',
                        alex_scd = 0}) of Left err -> Left err
                                          Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either (Error Span) (AlexState, a) }

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of 
                                Left err -> Left err
                                Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s,a)

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_inp=inp} -> 
        Right (s, (pos,c,inp))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,inp)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_inp=inp} of
                  s@(AlexState{}) -> Right (s, ())

alexError :: Error Span -> Alex a
alexError err = Alex $ \s -> Left err

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

alexMonadScan = do
  inp@(pos,_,str) <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (pos,_,str) -> do
      let t    = [chr (fromIntegral (LBS.head str))]
      let msg  = T.pack $ "lexical error at '" ++ t ++ "'"
      let span = mkTokSpan pos (1 :: Int)
      alexError $ LexError span msg
    AlexSkip  inp' _ -> do
        alexSetInput inp'
        alexMonadScan
    AlexToken inp'@(_,_,str') len action -> do
        alexSetInput inp'
        action pos (ByteString.take (fromIntegral len) str)

-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Int -> Alex result

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip input len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code input len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

token :: (AlexInput -> Int -> token) -> AlexAction token
token t input len = return (t input len)
}

-- vim: set ft=alex ts=2 et:
