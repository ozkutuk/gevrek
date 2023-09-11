module Lua.AST where

import Data.Text (Text)

data Program = Program
  { decls :: [Declaration]
  , main :: Maybe Statement
  }

data Declaration
  = DeclFun FunDecl

type Var = Text

data FunDecl = FunDecl
  { name :: Text
  , args :: [Var]
  , body :: [Statement]
  }

data Statement
  = Return Expr
  | ExprStmt Expr

data Expr
  = Lit Int
  | Var Var
  | FunCall Text [Expr]
