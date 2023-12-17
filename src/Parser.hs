{-# LANGUAGE OverloadedStrings #-}

module Parser where

-- import AST (Expr (..), Lit (..), Bind(..))

import Control.Applicative (empty, (<**>), (<|>))
import Core (Bind (..), Expr (..), Lit (..), Module (..))
import Data.Bifunctor (first)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec ((<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as MPL

type Parser = MP.Parsec Void Text

newtype ParseError = ParseError {unParseError :: Text}

parse :: Text -> Either ParseError Module
parse = first (ParseError . T.pack . MP.errorBundlePretty) . MP.runParser parseModule ""

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = scan
  where
    scan = p <**> rst
    rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id
{-# INLINE chainl1 #-}

-- NOTE(ozkutuk): probably should generalize this to 'Expr a'
-- at some point
parseExpr :: Parser (Expr Text)
parseExpr = chainl1 parseExpr' (App <$ hwhitespace)

parseExpr' :: Parser (Expr Text)
parseExpr' =
  lit
    <|> (parseLambda <?> "lambda expression")
    <|> (parseLet <?> "let expression")
    <|> (Var <$> parseIdent)
  where
    lit = Lit <$> parseLit

parseLet :: Parser (Expr Text)
parseLet = do
  keyword "let"
  bind <- lexeme parseBind
  keyword "in"
  Let bind <$> parseExpr

keyword :: Text -> Parser ()
keyword s = void $ lexeme' $ MP.try (MPC.string s *> MP.notFollowedBy MPC.alphaNumChar)

parseLambda :: Parser (Expr Text)
parseLambda = do
  void $ MPC.char '\\'
  binder <- parseIdent
  void $ lexeme (MPC.string "->")
  Lam binder <$> parseExpr

-- TODO(ozkutuk): actually implement this, take care of keywords
parseIdent :: Parser Text
parseIdent = T.pack <$> lexeme (MP.some MPC.letterChar) <?> "identifier"

parseLit :: Parser Lit
parseLit = (parseBool <?> "boolean") <|> parseInt
  where
    parseBool =
      (LitBool True <$ MPC.string "true")
        <|> (LitBool False <$ MPC.string "false")

    -- NOTE(ozkutuk): unsigned for now
    parseInt = LitInt <$> MPL.decimal

parseBind :: Parser (Bind Text)
parseBind = do
  name <- parseIdent
  void $ lexeme (MPC.char '=')
  Bind name <$> parseExpr

parseModule :: Parser Module
parseModule =
  Module
    <$> MP.someTill (lexeme' parseBind) MP.eof

lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme hwhitespace

lexeme' :: Parser a -> Parser a
lexeme' = MPL.lexeme whitespace

-- NOTE(ozkutuk): no comments for now
whitespace :: Parser ()
whitespace = MPL.space MPC.space1 empty empty

hwhitespace :: Parser ()
hwhitespace = MPL.space MPC.hspace1 empty empty
