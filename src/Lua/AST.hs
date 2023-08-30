module Lua.AST where

import Data.Text (Text)

newtype Program = Program {unProgram :: [Declaration]}

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
