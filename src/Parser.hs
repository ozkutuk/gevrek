{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Control.Applicative (empty, (<|>), Alternative)
import Control.Monad (guard, (>=>))
import Core (Bind (..), Expr (..), Lit (..), Module (..))
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Functor (void, ($>))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec ((<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as MPL
import Token (Token (..))
import Token qualified
import qualified Lexer

data WithPos a = WithPos
  { startPos :: MP.SourcePos
  , endPos :: MP.SourcePos
  , tokenLength :: Int
  , tokenVal :: a
  }
  deriving (Eq, Ord, Show)

data TokenStream = TokenStream
  { input :: Text
  , tokens :: [WithPos Token]
  }

instance MP.Stream TokenStream where
  type Token TokenStream = WithPos Token
  type Tokens TokenStream = [WithPos Token]

  tokenToChunk :: Proxy TokenStream -> MP.Token TokenStream -> MP.Tokens TokenStream
  tokenToChunk _ x = [x]

  tokensToChunk :: Proxy TokenStream -> [MP.Token TokenStream] -> MP.Tokens TokenStream
  tokensToChunk _ = id

  chunkToTokens :: Proxy TokenStream -> MP.Tokens TokenStream -> [MP.Token TokenStream]
  chunkToTokens _ = id

  chunkLength :: Proxy TokenStream -> MP.Tokens TokenStream -> Int
  chunkLength = undefined

  take1_ :: TokenStream -> Maybe (MP.Token TokenStream, TokenStream)
  take1_ ts = case ts.tokens of
    [] -> Nothing
    (t : ts') ->
      let srcRest = T.drop (MP.tokensLength (Proxy @TokenStream) (NE.singleton t)) ts.input
       in Just (t, TokenStream srcRest ts')

  takeN_ :: Int -> TokenStream -> Maybe (MP.Tokens TokenStream, TokenStream)
  takeN_ n ts
    | n <= 0 = Just ([], ts)
    | null ts.tokens = Nothing
    | otherwise =
        let (x, s') = splitAt n ts.tokens
         in case NE.nonEmpty x of
              Nothing -> Just (x, TokenStream ts.input s')
              Just nex -> Just (x, TokenStream (T.drop (MP.tokensLength (Proxy @TokenStream) nex) ts.input) s')

  takeWhile_ :: (MP.Token TokenStream -> Bool) -> TokenStream -> (MP.Tokens TokenStream, TokenStream)
  takeWhile_ f ts =
    let (x, tokens') = List.span f ts.tokens
     in case NE.nonEmpty x of
          Nothing -> (x, ts {tokens = tokens'})
          Just nex -> (x, TokenStream (T.drop (MP.tokensLength (Proxy @TokenStream) nex) ts.input) tokens')

instance MP.VisualStream TokenStream where
  showTokens :: Proxy TokenStream -> NE.NonEmpty (MP.Token TokenStream) -> String
  showTokens _ ts = unwords $ NE.toList $ fmap showToken ts
    where
      showToken :: WithPos Token -> String
      showToken = T.unpack . Token.showToken . (.tokenVal)

  tokensLength :: Proxy TokenStream -> NE.NonEmpty (MP.Token TokenStream) -> Int
  tokensLength _ = sum . fmap (.tokenLength)

instance MP.TraversableStream TokenStream where
  reachOffset :: Int -> MP.PosState TokenStream -> (Maybe String, MP.PosState TokenStream)
  reachOffset o MP.PosState {..} =
    ( Just $ T.unpack (prefix <> restOfLine)
    , MP.PosState
        { pstateInput = TokenStream
            { input = postStr
            , tokens = post
            }
        , pstateOffset = max pstateOffset o
        , pstateSourcePos = newSourcePos
        , pstateTabWidth = pstateTabWidth
        , pstateLinePrefix = T.unpack prefix
        }
    )
    where
      prefix =
        if sameLine
          then T.pack pstateLinePrefix <> preLine
          else preLine
      sameLine = MP.sourceLine newSourcePos == MP.sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> case pstateInput.tokens of
            [] -> pstateSourcePos
            xs -> endPos (last xs)
          (x:_) -> startPos x
      (pre, post) = splitAt (o - pstateOffset) pstateInput.tokens
      (preStr, postStr) = T.splitAt tokensConsumed pstateInput.input
      preLine = T.reverse . T.takeWhile (/= '\n') . T.reverse $ preStr
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> MP.tokensLength (Proxy @TokenStream) nePre
      restOfLine = T.takeWhile (/= '\n') postStr

type Parser = MP.Parsec Void TokenStream

newtype ParseError = ParseError {unParseError :: Text}
  deriving stock (Eq, Show)

liftToken :: Token -> WithPos Token
liftToken = WithPos pos pos 0
  where
    pos = MP.initialPos ""

token :: Token -> Parser Token
token t = MP.token test (Set.singleton . MP.Tokens . NE.singleton . liftToken $ t)
  where
    test :: WithPos Token -> Maybe Token
    test t' = guarded (== t'.tokenVal) t

guarded :: Alternative f => (b -> Bool) -> b -> f b
guarded p x = guard (p x) $> x

number :: Parser Int
number = MP.token test Set.empty <?> "number"
  where
    test (WithPos _ _ _ (TokNumber n)) = Just n
    test _ = Nothing

ident :: Parser Text
ident = MP.token test Set.empty <?> "identifier"
  where
    test (WithPos _ _ _ (TokIdent s)) = Just s
    test _ = Nothing

eof :: Parser ()
eof = void (token TokEof <?> "end-of-file")

block :: Parser a -> Parser a
block p = token TokLeftBrace *> p <* token TokRightBrace

parseBlockItems :: Parser a -> Parser [a]
parseBlockItems p = block $ MP.sepBy p (token TokSemicolon)

parseModule :: Parser Module
parseModule = Module <$> parseBlockItems parseBind

parseExpr :: Parser (Expr Text)
parseExpr =
  (parseLambda <?> "lambda expression")
  <|> (parseLet <?> "let expression")
  <|> (parseApp <?> "application")
  <|> prim

parseLambda :: Parser (Expr Text)
parseLambda = do
  void $ token TokBackslash
  binder <- ident
  void $ token TokRightArrow
  Lam binder <$> parseExpr

parseApp :: Parser (Expr Text)
parseApp = do
  firstPrim <- prim
  rest <- MP.many prim
  pure $ foldl' App firstPrim rest

parseLet :: Parser (Expr Text)
parseLet = do
  void $ token TokLet
  bind <- parseBind
  void $ token TokIn
  Let bind <$> parseExpr

parseBind :: Parser (Bind Text)
parseBind = do
  name <- ident
  void $ token TokEquals
  Bind name <$> parseExpr

parseLit :: Parser Lit
parseLit = LitInt <$> number

prim :: Parser (Expr Text)
prim =
  Var <$> ident
    <|> Lit <$> parseLit
    <|> parens parseExpr

parens :: Parser a -> Parser a
parens = MP.between (token TokLeftParen) (token TokRightParen)

data AnyError = ErrorLex Lexer.LexError | ErrorParse ParseError
  deriving stock (Show)

showError :: AnyError -> Text
showError (ErrorLex (Lexer.LexError s)) = "lex error: " <> s
showError (ErrorParse (ParseError s)) = "parse error: " <> s

-- TODO(ozkutuk): collect source positions during lexing stage
-- and then remove this
-- unsafeParse :: Text -> Either AnyError (Expr Text)
unsafeParse :: Text -> Either AnyError Module
unsafeParse src =
  (first ErrorLex . Lexer.lex) src
    >>= (first ErrorParse . parse . TokenStream src . map liftToken)

-- parse :: TokenStream -> Either ParseError (Expr Text)
parse :: TokenStream -> Either ParseError Module
parse = parse' (parseModule <* eof)
-- parse = parse' (parseExpr <* token TokEof)
-- parse = parse' (parseExpr <* MP.eof)

parse' :: Parser a -> TokenStream -> Either ParseError a
parse' p = first (ParseError . T.pack . MP.errorBundlePretty) . MP.runParser p ""
--
-- parseExpr :: Parser (Expr Text)
-- parseExpr =
--   (parseLambda <?> "lambda expression")
--     <|> (parseLet <?> "let expression")
--     <|> parseApp
--
-- parseApp :: Parser (Expr Text)
-- parseApp = do
--   firstPrim <- lexeme prim
--   rest <- MP.many (lexeme prim)
--   pure $ foldl' App firstPrim rest
--
-- parens :: Parser a -> Parser a
-- parens = MP.between (symbol "(") (symbol ")")
--
-- prim :: Parser (Expr Text)
-- prim =
--   Var <$> parseIdent
--     <|> Lit <$> parseLit
--     <|> parens parseExpr
--
-- parseLet :: Parser (Expr Text)
-- parseLet = do
--   keyword "let"
--   bind <- lexeme parseBind
--   keyword "in"
--   Let bind <$> parseExpr
--
-- keyword :: Text -> Parser ()
-- keyword s = void $ lexeme $ MP.try (MPC.string s *> MP.notFollowedBy MPC.alphaNumChar)
--
-- parseLambda :: Parser (Expr Text)
-- parseLambda = do
--   void $ MPC.char '\\'
--   binder <- parseIdent
--   void $ lexeme (MPC.string "->")
--   Lam binder <$> parseExpr
--
-- keywords :: Set Text
-- keywords =
--   Set.fromList
--     ["let", "in"]
--
-- -- TODO(ozkutuk): this needs to accept alphanum, underscores, etc.
-- parseIdent :: Parser Text
-- parseIdent = MP.try $ do
--   ident <- T.pack <$> lexeme (MP.some MPC.letterChar) <?> "identifier"
--   guard $ ident `Set.notMember` keywords
--   pure ident
--
-- parseLit :: Parser Lit
-- parseLit = (parseBool <?> "boolean") <|> parseInt
--   where
--     parseBool =
--       (LitBool True <$ MPC.string "true")
--         <|> (LitBool False <$ MPC.string "false")
--
--     -- NOTE(ozkutuk): unsigned for now
--     parseInt = LitInt <$> MPL.decimal
--
-- parseBind :: Parser (Bind Text)
-- parseBind = do
--   name <- parseIdent
--   void $ lexeme (MPC.char '=')
--   Bind name <$> parseExpr
--
-- lexeme :: Parser a -> Parser a
-- lexeme = MPL.lexeme whitespace
--
-- symbol :: Text -> Parser Text
-- symbol = MPL.symbol whitespace
--
-- -- NOTE(ozkutuk): no comments for now
-- whitespace :: Parser ()
-- whitespace = MPL.space MPC.space1 empty empty
