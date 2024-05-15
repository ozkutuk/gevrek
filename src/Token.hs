module Token where

import Data.Text (Text)

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
  | TokEof
  -- Layout
  | TokLeftBrace
  | TokRightBrace
  | TokSemicolon
  deriving stock (Eq, Show)
