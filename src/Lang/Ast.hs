module Lang.Ast where

newtype Id = Id String deriving (Eq, Ord, Show, Read)

data Literal 
    = LitInt Integer
    | LitBool Bool
    deriving (Eq, Ord, Show, Read)

data Expr 
    = Var Id
    | Lit Literal
    | App Expr Expr
    | Lam Id Type Expr
    deriving (Eq, Ord, Show, Read)

data Type
    = TInt
    | TBool
    | TFun Type Type
    deriving (Eq, Ord, Show, Read)

{-checkLiteral :: Map Id Type -> Literal -> Type -> Maybe (Map Id Type)
checkLiteral ctx (LitInt _) TInt = return ctx
checkLiteral _ (LitInt _) _ = Nothing
checkLiteral ctx (LitBool _) TBool = return ctx
checkLiteral _ (LitBool _) _ = Nothing

inferLiteral :: Map Id Type -> Literal -> Maybe Type
inferLiteral _ (LitInt _) = return TInt
inferLiteral _ (LitBool _) = return TBool

checkExpr :: Map Id Type -> Expr -> Type -> Maybe (Map Id Type)
checkExpr ctx (Var var) t = case M.lookup var ctx of
    Just t' -> if t' == t then return ctx else Nothing
    Nothing -> Nothing
checkExpr ctx (Lit k) t = checkLiteral ctx k t
checkExpr ctx (App e1 e2) t = case inferExpr ctx e1 of
    Just (TFun t1 t2) -> if t2 == t then checkExpr ctx e2 t1 >> return ctx else Nothing
    _ -> Nothing
checkExpr ctx (Lam var t1 e) (TFun t2 t3) = if t1 == t2 then checkExpr (M.insert var t1 ctx) e t3 >> return ctx else Nothing
checkExpr _ Lam{} _ = Nothing

inferExpr :: Map Id Type -> Expr -> Maybe Type
inferExpr ctx (Var var) = M.lookup var ctx
inferExpr ctx (Lit k) = inferLiteral ctx k
inferExpr ctx (App e1 e2) = case inferExpr ctx e1 of
    Just (TFun t1 t2) -> checkExpr ctx e2 t1 >> return t2
    _ -> Nothing
inferExpr ctx (Lam var t1 e) = do
    t2 <- inferExpr (M.insert var t1 ctx) e
    return (TFun t1 t2)-}
    