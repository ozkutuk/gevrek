{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO qualified as T
import Lua.AST
  ( Declaration (..)
  , Expr (..)
  , FunDecl (..)
  , Program (..)
  , Statement (..)
  )
import Lua.Pretty (render)
import System.Environment (getArgs)

program :: Program
program =
  Program
    [DeclFun (FunDecl "main" ["x", "y"] [Return (Var "x")])]
    (Just (ExprStmt (FunCall "main" [Lit 42, Lit 24])))

main :: IO ()
main = do
  T.putStrLn $ render program

-- args <- getArgs
-- let f = head args
-- contents <- T.readFile f
-- T.putStrLn contents
