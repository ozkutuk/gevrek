module Core where

import Data.Text (Text)

data Expr a
  = Var Text
  | Lit Lit
  | App (Expr a) (Expr a)
  | Lam a (Expr a)
  | Let (Bind a) (Expr a)
  | Case (Expr a) [CaseAlternative a]

data CaseAlternative a = CaseAlternative
  { binder :: Binder a
  , result :: Expr a
  }

-- NOTE(ozkutuk): the type param will probably be
-- needed once we add more complex binders like array literals etc.
data Binder a
  = WildcardBinder
  | VarBinder Text
  | LitBinder Lit

data Lit
  = LitInt Int
  | LitBool Bool

data Bind a = Bind a (Expr a)

newtype Module = Module {unModule :: [Bind Text]}
