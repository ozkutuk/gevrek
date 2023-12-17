{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO qualified as T

-- import Lua.AST
--   ( Declaration (..)
--   , Expr (..)
--   , FunDecl (..)
--   , Program (..)
--   , Statement (..)
--   )

import Core
  ( Bind (..)
  , Binder (..)
  , CaseAlternative (..)
  , Expr (..)
  , Lit (..)
  , Module (..)
  )
import CoreToLua (coreToLua)
import Data.Text (Text)
import Debug.Trace (traceShow)
import Lua.Pretty (render)
import Parser (ParseError (..), parse)
import System.Environment (getArgs)

-- coreModule :: Module
-- coreModule =
--   Module
--     [ Bind "foo" $
--         Let (Bind "x" (Lit (LitInt 5))) $
--           Let (Bind "y" (Lit (LitBool False))) $
--             Var "x"
--     , Bind "cnst" $
--         Lam "x" $
--           Lam "y" $
--             Var "x"
--     ]
--
-- coreModule2 :: Module
-- coreModule2 =
--   Module
--     [ Bind "isOne" $
--         Lam "num" $
--           Case
--             (Var "num")
--             [ CaseAlternative (LitBinder (LitInt 1)) (Lit (LitBool True))
--             , CaseAlternative WildcardBinder (Lit (LitBool False))
--             ]
--     , Bind "foo" $
--         Lam "n" $
--           Case
--             (Var "n")
--             [ CaseAlternative (LitBinder (LitInt 1)) (Lit (LitInt 0))
--             , CaseAlternative (LitBinder (LitInt 2)) (Lit (LitInt 5))
--             , CaseAlternative
--                 (VarBinder "n")
--                 (App (App (Var "add") (Var "n")) (Var "n"))
--             ]
--     ]

-- program :: Program
-- program =
--   Program
--     [DeclFun (FunDecl "main" ["x", "y"] [Return (Var "x")])]
--     (Just (ExprStmt (FunCall "main" [Lit 42, Lit 24])))

compile :: Module -> Text
compile = render . coreToLua

compileWithPrelude :: Module -> IO Text
compileWithPrelude md = do
  let compiled = compile md
  prelude <- T.readFile "Prelude.lua"
  pure $ prelude <> "\n" <> compiled

main :: IO ()
main = do
  args <- getArgs
  let f = head args
  program <- T.readFile f
  let eParsed = parse program
  case eParsed of
    Left e -> T.putStrLn (unParseError e)
    Right parsed ->
      traceShow parsed $
        T.putStrLn =<< compileWithPrelude parsed

-- args <- getArgs
-- let f = head args
-- contents <- T.readFile f
-- T.putStrLn contents
