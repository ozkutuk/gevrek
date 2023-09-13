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
import Lua.Pretty (render)
import System.Environment (getArgs)
import Core (Module(..))
import Core (Bind(..))
import Core (Expr(..))
import Core (Lit(..))
import CoreToLua (coreToLua)

coreModule :: Module
coreModule =
  Module
    [ Bind "foo" $
        Let (Bind "x" (Lit (LitInt 5))) $
          Let (Bind "y" (Lit (LitBool False))) $
            Var "x"
    , Bind "cnst" $
        Lam "x" $
          Lam "y" $
            Var "x"
    ]

-- program :: Program
-- program =
--   Program
--     [DeclFun (FunDecl "main" ["x", "y"] [Return (Var "x")])]
--     (Just (ExprStmt (FunCall "main" [Lit 42, Lit 24])))

main :: IO ()
main = do
  -- T.putStrLn $ render program
  T.putStrLn $ render $ coreToLua coreModule

-- args <- getArgs
-- let f = head args
-- contents <- T.readFile f
-- T.putStrLn contents
