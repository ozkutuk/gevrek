module Core where

import Data.Text (Text)

data Expr a
  = Var Text
  | Lit Lit
  | App (Expr a) (Expr a)
  | Lam a (Expr a)
  | Let (Bind a) (Expr a)
  | Case (Expr a) [CaseAlternative a]
  deriving (Show)

data CaseAlternative a = CaseAlternative
  { binder :: Binder a
  , result :: Expr a
  }
  deriving (Show)

-- NOTE(ozkutuk): the type param will probably be
-- needed once we add more complex binders like array literals etc.
data Binder a
  = WildcardBinder
  | VarBinder Text
  | LitBinder Lit
  deriving (Show)

data Lit
  = LitInt Int
  | LitBool Bool
  deriving (Eq, Show)

data Bind a = Bind a (Expr a)
  deriving (Show)

newtype Module = Module {unModule :: [Bind Text]}
  deriving (Show)
