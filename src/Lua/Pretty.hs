{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lua.Pretty where

import Lua.AST (Declaration (..), FunDecl (..), Program (..), Statement (..), Expr (..))
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP

prettyProgram :: Program -> Doc ann
prettyProgram (Program decls) = PP.vsep $ map prettyDecl decls

prettyDecl :: Declaration -> Doc ann
prettyDecl (DeclFun funDecl) = prettyFunDecl funDecl

block :: Doc ann -> Doc ann
block = PP.enclose PP.lbrace PP.rbrace

prettyFunDecl :: FunDecl -> Doc ann
prettyFunDecl fun =
  "function"
    <+> PP.pretty fun.name
    <+> PP.tupled (map PP.pretty fun.args)
    <+> block (PP.vsep $ map prettyStatement fun.body)

prettyStatement :: Statement -> Doc ann
prettyStatement (Return e) = "return" <+> prettyExpr e
prettyStatement (ExprStmt e) = undefined

prettyExpr :: Expr -> Doc ann
prettyExpr (Lit n) = PP.pretty n
prettyExpr (Var v) = PP.pretty v

