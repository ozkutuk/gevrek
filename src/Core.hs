module Core where
import Data.Text (Text)

data Core a
  = Var Text
  | Lit Lit
  | App (Core a) (Core a)
  | Lam a (Core a) 
  | Let (Bind a) (Core a)

data Lit
  = LitInt Int

data Bind a = Bind a (Core a)
