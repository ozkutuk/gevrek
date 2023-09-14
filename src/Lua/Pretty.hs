{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lua.Pretty where

import Data.Text (Text)
import Lua.AST (Declaration (..), Expr (..), FunDecl (..), Lit (..), Program (..), Statement (..), ValDecl (..))
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
prettyDecl (DeclVal valDecl) = prettyValDecl valDecl

prettyValDecl :: ValDecl -> Doc ann
prettyValDecl (ValDecl name value) = prettyAssign name value

prettyAssign :: Text -> Expr -> Doc ann
prettyAssign name value =
  PP.pretty name <+> PP.equals <+> prettyExpr value

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
prettyStatement (DeclStmt decl) = prettyDecl decl

prettyExpr :: Expr -> Doc ann
prettyExpr (Lit lit) = prettyLit lit
prettyExpr (Var v) = PP.pretty v
prettyExpr (FunCall fun args) =
  PP.parens (prettyExpr fun) <> PP.tupled (map prettyExpr args)
prettyExpr (Fun params body) =
  PP.vsep
    [ "function"
        <> PP.tupled (map PP.pretty params)
    , PP.indent 4 (prettyExpr body)
    , "end"
    ]
prettyExpr (Block stmts) = PP.vsep (map prettyStatement stmts)
prettyExpr (Table items) = PP.braces $ PP.tupled $ map (uncurry prettyAssign) items
prettyExpr (If cond body) =
  PP.vsep
    [ "if" <+> prettyExpr cond <+> "then"
    , PP.indent 4 (prettyExpr body)
    , "end"
    ]
prettyExpr (Equals e1 e2) = prettyExpr e1 <+> "==" <+> prettyExpr e2

prettyLit :: Lit -> Doc ann
prettyLit (LitInt n) = PP.pretty n
prettyLit (LitBool True) = "true"
prettyLit (LitBool False) = "false"
