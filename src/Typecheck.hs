{-# LANGUAGE OverloadedStrings #-}

module Typecheck where

import Core (Expr (..), Lit (..), Ty (..), Bind (..), Module (..), CaseAlternative (..), Binder (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Control.Monad (foldM_, guard, unless)
import Debug.Trace (traceM)
import Data.Foldable (traverse_)
import qualified Data.Set as Set

typecheck :: Module -> Either Text ()
typecheck (Module bindings) = foldM_ addBind preludeCtx bindings

addBind :: Context -> Bind Text -> Either Text Context
addBind ctx@(Context ctx') bind@(Bind v _) = do
  -- traceM $ show ctx'
  t <- typecheckBind ctx bind
  pure $ extendCtx ctx v t

typecheckBind :: Context -> Bind Text -> Either Text Ty
typecheckBind ctx (Bind _ e) = typecheckExpr ctx e

newtype Context = Context (Map Text Ty)

preludeCtx :: Context
preludeCtx = Context $ Map.fromList
  [ ("and", Fun Bool (Fun Bool Bool))
  , ("or", Fun Bool (Fun Bool Bool))
  , ("not", Fun Bool Bool)
  , ("add", Fun Number (Fun Number Number))
  , ("sub", Fun Number (Fun Number Number))
  , ("div", Fun Number (Fun Number Number))
  , ("mul", Fun Number (Fun Number Number))
  ]

emptyCtx :: Context
emptyCtx = Context Map.empty

lookupCtx :: Text -> Context -> Maybe Ty
lookupCtx v (Context ctx) = Map.lookup v ctx

extendCtx :: Context -> Text -> Ty -> Context
extendCtx (Context ctx) v ty = Context $ Map.insert v ty ctx

note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e Nothing = Left e

typecheckLit :: Lit -> Ty
typecheckLit = \case
  LitInt _ -> Number
  LitBool _ -> Bool

typecheckExpr :: Context -> Expr Text -> Either Text Ty
typecheckExpr ctx = \case
  Var v -> note ("variable not found: " <> v) $ lookupCtx v ctx
  Lit lit -> Right $ typecheckLit lit
  App e1 e2 -> do
    t1 <- typecheckExpr ctx e1
    case t1 of
      Fun t11 t12 -> do
        t2 <- typecheckExpr ctx e2
        if t2 == t11
          then Right t12
          else Left $ "expected type: " <> showType t11 <> ", actual type: " <> showType t2
      otherTy -> Left $ "expected a function type, found: " <> showType otherTy
  Lam v ty e -> do
    Fun ty <$> typecheckExpr (extendCtx ctx v ty) e
  Let (Bind v e1) e2 -> do
    t1 <- typecheckExpr ctx e1
    typecheckExpr (extendCtx ctx v t1) e2
  Case scrutinee caseAlts -> do
    t' <- typecheckExpr ctx scrutinee
    altTys <- traverse (typecheckCaseAlt ctx t') caseAlts
    unless (length (Set.fromList altTys) == 1) $
      Left "case alt types differ"
    pure $ head altTys

typecheckCaseAlt :: Context -> Ty -> CaseAlternative Text -> Either Text Ty
typecheckCaseAlt ctx scrutineeTy (CaseAlternative binder result) = do
  ctx' <- binderTyMatches ctx scrutineeTy binder
  typecheckExpr ctx' result

binderTyMatches :: Context -> Ty -> Binder Text -> Either Text Context
binderTyMatches ctx expectedTy = \case
  WildcardBinder -> pure ctx
  VarBinder v -> pure (extendCtx ctx v expectedTy)
  LitBinder lit ->
    if typecheckLit lit == expectedTy
      then pure ctx
      else Left $ "scrutinee type (" <> showType expectedTy <>
        ") does not match the literal type (" <> showType (typecheckLit lit) <> ")"

-- >>> showType $ Fun Bool (Fun Number Bool)
-- "(Bool -> (Number -> Bool))"
showType :: Ty -> Text
showType Bool = "Bool"
showType Number = "Number"
showType (Fun t1 t2) = "(" <> showType t1 <> " -> " <> showType t2 <> ")"

