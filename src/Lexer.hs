{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as MPL

import Control.Applicative (Alternative (..), (<|>))
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Text qualified as Text
import Token (Token (..))

type Lexer = Parsec Void Text

newtype LexError = LexError Text
  deriving newtype (Show)

lex :: Text -> Either LexError [Token]
lex = first (LexError . Text.pack . MP.errorBundlePretty) . MP.runParser tokens ""

tokens :: Lexer [Token]
tokens = do
  space
  tokens' <- many (token <* space)
  MP.eof
  pure $ tokens' <> [TokEof]
  where
    space = MPL.space MPC.space1 (MPL.skipLineComment "--") empty

token :: Lexer Token
token =
  MPC.string "case" $> TokCase
    <|> MPC.string "of" $> TokOf
    <|> MPC.string "let" $> TokLet
    <|> MPC.string "in" $> TokIn
    <|> MPC.char '=' $> TokEquals
    <|> MPC.char '\\' $> TokBackslash
    <|> MPC.string "->" $> TokRightArrow
    -- <|> MP.eof $> TokEof
    <|> MPC.char '{' $> TokLeftBrace
    <|> MPC.char '}' $> TokRightBrace
    <|> MPC.char ';' $> TokSemicolon
    <|> number
    <|> ident
    <|> operator

number :: Lexer Token
number = TokNumber <$> MPL.decimal

ident :: Lexer Token
ident = do
  hd <- MPC.lowerChar
  tl <- many MPC.alphaNumChar
  pure $ TokIdent $ Text.pack (hd : tl)

operator :: Lexer Token
operator = TokOperator . Text.pack <$> some symbolChar
  where
    symbolChar = MPC.symbolChar <|> MP.oneOf ['-', '*', '/']
