{-# LANGUAGE OverloadedStrings #-}

module Token where

import Data.Text (Text)
import qualified Data.Text as T

-- TODO(ozkutuk): add parens
data Token
  = TokIdent Text
  | TokNumber Int
  | TokOperator Text
  | TokCase
  | TokOf
  | TokLet
  | TokIn
  | TokEquals
  | TokBackslash
  | TokRightArrow
  | TokLeftParen
  | TokRightParen
  | TokUnderscore
  | TokEof
  -- Layout
  | TokLeftBrace
  | TokRightBrace
  | TokSemicolon
  deriving stock (Eq, Ord, Show)

showToken :: Token -> Text
showToken = \case
  TokIdent ident -> ident
  TokNumber number -> T.pack (show number)
  TokOperator op -> op
  TokCase -> "case"
  TokOf -> "of"
  TokLet -> "let"
  TokIn -> "in"
  TokEquals -> "="
  TokBackslash -> "\\"
  TokRightArrow -> "->"
  TokLeftParen -> "("
  TokRightParen -> ")"
  TokUnderscore -> "_"
  TokEof -> "EOF"
  TokLeftBrace -> "{"
  TokRightBrace -> "}"
  TokSemicolon -> ";"
