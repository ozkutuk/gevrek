{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lua.Pretty where

import Data.Text (Text)
import Lua.AST (Declaration (..), Expr (..), FunDecl (..), Program (..), Statement (..))
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP

render :: Program -> Text
render = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions . prettyProgram

prettyProgram :: Program -> Doc ann
prettyProgram program =
  PP.vsep (map prettyDecl program.decls)
    <> maybe mempty (((PP.line <> PP.line) <>) . prettyStatement) program.main

prettyDecl :: Declaration -> Doc ann
prettyDecl (DeclFun funDecl) = prettyFunDecl funDecl

prettyFunDecl :: FunDecl -> Doc ann
prettyFunDecl fun =
  PP.vsep
    [ "function"
        <+> PP.pretty fun.name
        <+> PP.tupled (map PP.pretty fun.args)
    , PP.indent 4 (PP.vsep (map prettyStatement fun.body))
    , "end"
    ]

prettyStatement :: Statement -> Doc ann
prettyStatement (Return e) = "return" <+> prettyExpr e
prettyStatement (ExprStmt e) = prettyExpr e

prettyExpr :: Expr -> Doc ann
prettyExpr (Lit n) = PP.pretty n
prettyExpr (Var v) = PP.pretty v
prettyExpr (FunCall fun args) =
  PP.pretty fun <> PP.tupled (map prettyExpr args)
