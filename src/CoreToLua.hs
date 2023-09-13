module CoreToLua where

import Core (Bind (..), Expr (..), Lit (..), Module (..))
import Data.Text (Text)
import Lua.AST qualified as Lua

coreToLua :: Module -> Lua.Program
coreToLua (Module binds) = Lua.Program (map bindToLua binds) Nothing

bindToLua :: Bind Text -> Lua.Declaration
bindToLua (Bind ident expr) = Lua.DeclVal (Lua.ValDecl ident (exprToLua expr))

exprToLua :: Expr Text -> Lua.Expr
exprToLua (Var var) = Lua.Var var
exprToLua (Lit lit) = Lua.Lit (litToLua lit)
-- TODO(ozkutuk): something to be done about currying/multiple args etc
exprToLua (App e1 e2) = Lua.FunCall (exprToLua e1) [exprToLua e2]
exprToLua (Lam x e) = Lua.Fun [x] (Lua.Block [Lua.Return (exprToLua e)])
exprToLua (Let bind e) =
  let e' = exprToLua e
      bind' = bindToLua bind
   in Lua.FunCall
        (Lua.Fun []
          (Lua.Block [Lua.DeclStmt bind', Lua.Return e'])) []

litToLua :: Lit -> Lua.Lit
litToLua (LitInt n) = Lua.LitInt n
litToLua (LitBool b) = Lua.LitBool b

-- coreToLuaDecls :: Core Text -> [Lua.Declaration]
-- coreToLuaDecls = _
