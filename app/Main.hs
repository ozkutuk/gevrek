{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO qualified as T
import Lua.AST (Declaration (..), FunDecl (..), Program (..), Statement (..))
import System.Environment (getArgs)

program :: Program
program = Program [DeclFun (FunDecl "main" ["x"] [Return (Var "x")])]

main :: IO ()
main = do
  args <- getArgs
  let f = head args
  contents <- T.readFile f
  T.putStrLn contents
