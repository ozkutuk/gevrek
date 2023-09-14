{-# LANGUAGE DuplicateRecordFields #-}

module Lua.AST where

import Data.Text (Text)

data Program = Program
  { decls :: [Declaration]
  , main :: Maybe Statement
  }

data Declaration
  = DeclFun FunDecl
  | DeclVal ValDecl

type Var = Text

data FunDecl = FunDecl
  { name :: Text
  , args :: [Var]
  , body :: [Statement]
  }

data ValDecl = ValDecl
  { name :: Text
  , value :: Expr
  }

data Statement
  = Return Expr
  | ExprStmt Expr
  | DeclStmt Declaration

data Expr
  = Lit Lit
  | Var Var
  | FunCall Expr [Expr]
  | Fun [Text] Expr
  | -- is this _really_ an expression?
    -- maybe I can rename the datatype to AST or
    -- something other than Expr...
    Block [Statement]
  | -- | LetBlock [Declaration] Expr
    Table [(Text, Expr)]
  | If Expr Expr
  | Equals Expr Expr

data Lit
  = LitInt Int
  | LitBool Bool
