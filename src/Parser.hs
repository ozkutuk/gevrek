{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative (empty, (<|>))
import Control.Monad (guard)
import Core (Bind (..), Expr (..), Lit (..))
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Functor (void)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec ((<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as MPL

type Parser = MP.Parsec Void Text

newtype ParseError = ParseError {unParseError :: Text}

parse :: Text -> Either ParseError (Expr Text)
parse = first (ParseError . T.pack . MP.errorBundlePretty) . MP.runParser (parseExpr <* MP.eof) ""

parseExpr :: Parser (Expr Text)
parseExpr =
  (parseLambda <?> "lambda expression")
    <|> (parseLet <?> "let expression")
    <|> parseApp

parseApp :: Parser (Expr Text)
parseApp = do
  firstPrim <- lexeme prim
  rest <- MP.many (lexeme prim)
  pure $ foldl' App firstPrim rest

parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

prim :: Parser (Expr Text)
prim =
  Var <$> parseIdent
    <|> Lit <$> parseLit
    <|> parens parseExpr

parseLet :: Parser (Expr Text)
parseLet = do
  keyword "let"
  bind <- lexeme parseBind
  keyword "in"
  Let bind <$> parseExpr

keyword :: Text -> Parser ()
keyword s = void $ lexeme $ MP.try (MPC.string s *> MP.notFollowedBy MPC.alphaNumChar)

parseLambda :: Parser (Expr Text)
parseLambda = do
  void $ MPC.char '\\'
  binder <- parseIdent
  void $ lexeme (MPC.string "->")
  Lam binder <$> parseExpr

keywords :: Set Text
keywords =
  Set.fromList
    ["let", "in"]

-- TODO(ozkutuk): this needs to accept alphanum, underscores, etc.
parseIdent :: Parser Text
parseIdent = MP.try $ do
  ident <- T.pack <$> lexeme (MP.some MPC.letterChar) <?> "identifier"
  guard $ ident `Set.notMember` keywords
  pure ident

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

lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme whitespace

symbol :: Text -> Parser Text
symbol = MPL.symbol whitespace

-- NOTE(ozkutuk): no comments for now
whitespace :: Parser ()
whitespace = MPL.space MPC.space1 empty empty
