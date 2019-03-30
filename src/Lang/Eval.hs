module Lang.Eval where

import Lang.Ast
import Lang.Pretty
import JetErrorM
import JetContext
import Data.Map (Map)
import qualified Data.Map as M

import Debug.Trace

type EvalContext = JetContextMap Id Expr

subst :: Id -> Expr -> Expr -> Expr
subst var e e'@(Var _ var') | var == var' = e
                            | otherwise = e'
subst _ _ e@Lit{} = e
subst var e (App e1 e2) = App (subst var e e1) (subst var e e2)
subst var e e'@(Lam var' t e1)  | var == var' = e'
                                | otherwise = Lam var' t (subst var e e1)
subst var e (Let var' e1 e2)    | var == var' = Let var' (subst var e e1) e2
                                | otherwise = Let var' (subst var e e1) (subst var e e2)
subst var e (If e1 e2 e3) = If (subst var e e1) (subst var e e2) (subst var e e3)
subst var e (Pred e1) = Pred (subst var e e1)
subst var e (ESucc e1) = ESucc (subst var e e1)
subst var e (Iszero e1) = Iszero (subst var e e1)
subst var e (Fix e1) = Fix (subst var e e1)

isValue :: Expr -> Bool
isValue Lit{} = True
isValue Lam{} = True
isValue _ = False

evalExpr :: EvalContext -> Expr -> JetError Expr
evalExpr ctx e = case go ctx e of
    Succ e' | e == e' -> return e
            | otherwise -> evalExpr ctx e'
    Fail err -> Fail err
    where
        go :: EvalContext -> Expr -> JetError Expr
        go ctx (Var _ var) = lookupContext var ctx
        go _ e@Lit{} = return e 
        go _ (If (Lit _ (LitBool True)) e1 e2) = return e1
        go _ (If (Lit _ (LitBool False)) e1 e2) = return e2
        go ctx (If ep e1 e2) = do
            ep' <- go ctx ep
            return (If ep' e1 e2)
        go _ (Pred (Lit loc (LitInt n))) = return (Lit loc (LitInt (n - 1)))
        go ctx (Pred e) = do
            e' <- go ctx e
            return (Pred e')
        go _ (ESucc (Lit loc (LitInt n))) = return (Lit loc (LitInt (n + 1)))
        go ctx (ESucc e) = do
            e' <- go ctx e
            return (ESucc e')
        go _ (Iszero (Lit loc (LitInt n))) = return (Lit loc (LitBool (n == 0)))
        go ctx (Iszero e) = do
            e' <- go ctx e
            return (Iszero e')
        go ctx e@Lam{} = return e
        go ctx (App el@(Lam var t e1) e2) = 
            if isValue e2 then return (subst var e2 e1)
            else do
                e2' <- go ctx e2
                return (App el e2')
        go ctx (App e1 e2) = do
            e1' <- go ctx e1
            return (App e1' e2)
        go ctx (Let var e1 e2) =
            if isValue e1 then return (subst var e1 e2)
            else do
                e1' <- go ctx e1
                return (Let var e1' e2)
        go ctx ef@(Fix (Lam var t e)) = return (subst var ef e)
        go ctx (Fix e) = do
            e' <- go ctx e
            return (Fix e')

getValue :: Expr -> JetError Literal
getValue (Lit _ lit) = return lit
getValue e = fail "Expression not value"
