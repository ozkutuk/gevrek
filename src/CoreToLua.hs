{-# LANGUAGE OverloadedStrings #-}

module CoreToLua where

import Core (Bind (..), Binder (..), CaseAlternative (..), Expr (..), Lit (..), Module (..))
import Data.Text (Text)
import Lua.AST qualified as Lua
import qualified Data.Text as T
import qualified Data.Set as Set

coreToLua :: Module -> Lua.Program
coreToLua (Module binds) = Lua.Program (map bindToLua binds) Nothing

bindToLua :: Bind Text -> Lua.Declaration
bindToLua (Bind ident expr) = Lua.DeclVal (Lua.ValDecl ident (exprToLua expr))

exprToLua :: Expr Text -> Lua.Expr
exprToLua (Var var) = Lua.Var (escapeReserved var)
exprToLua (Lit lit) = Lua.Lit (litToLua lit)
-- TODO(ozkutuk): something to be done about currying/multiple args etc
exprToLua (App e1 e2) = Lua.FunCall (exprToLua e1) [exprToLua e2]
exprToLua (Lam x _ e) = Lua.Fun [x] (Lua.Block [Lua.Return (exprToLua e)])
exprToLua (Let bind e) =
  let e' = exprToLua e
      bind' = bindToLua bind
   in Lua.FunCall
        ( Lua.Fun
            []
            (Lua.Block [Lua.DeclStmt bind', Lua.Return e'])
        )
        []
exprToLua (Case scrutinee cases) =
  Lua.FunCall
    ( Lua.Fun
        [scrutineeVar]
        (Lua.Block $ map (Lua.ExprStmt . caseAltToLua) cases)
    )
    [exprToLua scrutinee]

escapeReserved :: Lua.Var -> Lua.Var
escapeReserved var =
  if var `Set.member` luaReserved
    then escape var
    else var
  where
    escape :: Lua.Var -> Lua.Var
    escape v = T.snoc v '_'

luaReserved :: Set.Set Lua.Var
luaReserved = Set.fromList [ "and", "or", "not" ]

wrapDoBlock :: Lua.Statement -> Lua.Statement
wrapDoBlock x = Lua.DoBlock [x]

caseAltToLua :: CaseAlternative Text -> Lua.Expr
caseAltToLua (CaseAlternative binder result) = case binder of
  WildcardBinder -> Lua.Block [wrapDoBlock $ Lua.Return (exprToLua result)]
  VarBinder var ->
    Lua.Block
      [ Lua.DoBlock
          [ Lua.DeclStmt (Lua.DeclVal (Lua.ValDecl var (Lua.Var scrutineeVar)))
          , Lua.Return (exprToLua result)
          ]
      ]
  LitBinder lit ->
    Lua.If
      (Lua.Equals (Lua.Var scrutineeVar) (Lua.Lit (litToLua lit)))
      (Lua.Block [Lua.Return (exprToLua result)])

scrutineeVar :: Text
scrutineeVar = "__scrutinee"

litToLua :: Lit -> Lua.Lit
litToLua (LitInt n) = Lua.LitInt n
litToLua (LitBool b) = Lua.LitBool b

-- coreToLuaDecls :: Core Text -> [Lua.Declaration]
-- coreToLuaDecls = _
