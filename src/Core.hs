module Core where

import Data.Text (Text)

data Expr a
  = Var Text
  | Lit Lit
  | App (Expr a) (Expr a)
  | Lam a (Expr a)
  | Let (Bind a) (Expr a)

data Lit
  = LitInt Int
  | LitBool Bool

data Bind a = Bind a (Expr a)

newtype Module = Module {unModule :: [Bind Text]}
