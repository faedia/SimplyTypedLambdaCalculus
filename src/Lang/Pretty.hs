module Lang.Pretty where

import Lang.Ast

class Show a => PrettyShow a where
    prettyShow :: a -> String

instance PrettyShow Id where
    prettyShow (Id s) = s

instance PrettyShow Literal where
    prettyShow (LitBool False) = "false"
    prettyShow (LitBool True) = "true"
    prettyShow (LitInt n) = show n
    prettyShow LitUnit = "()"

instance PrettyShow Expr where
    prettyShow (Var _ var) = prettyShow var
    prettyShow (Lit _ lit) = prettyShow lit
    prettyShow (If ep et ef) = "if " ++ prettyShow ep ++ " then " ++ prettyShow et ++ " else " ++ prettyShow ef
    prettyShow (App e1 e2) = let 
        e1Str = let e1Str = prettyShow e1 in 
            if shouldParenExpr e1 then "(" ++ e1Str ++ ")" else e1Str;
        e2Str = let e2Str = prettyShow e2 in 
            if shouldParenExpr e2 then "(" ++ e2Str ++ ")" else e2Str; in
            e1Str ++ " " ++ e2Str
    prettyShow (Lam var t e) =
        "\\" ++ prettyShow var ++ " : " ++ prettyShow t ++ " . " ++ prettyShow e
    prettyShow (Let var e1 e2) = "let " ++ prettyShow var ++ " = " ++ prettyShow e1 ++ " in " ++ prettyShow e2
    prettyShow (Fix e) = "fix " ++ prettyShow e
    prettyShow (ESucc e) = "succ " ++ prettyShow e
    prettyShow (Pred e) = "pred " ++ prettyShow e
    prettyShow (Iszero e) = "iszero " ++ prettyShow e
    -- Here just incase a new Expr is added and a prettyShow function is not written for the new constructor
    prettyShow e = "Write prettyShow! defaulting to show: " ++ show e


instance PrettyShow Type where
    prettyShow TInt = "Int"
    prettyShow TBool = "Bool"
    prettyShow TUnit = "()"
    prettyShow (TFun t1 t2) = case t1 of
            TFun _ _ -> "(" ++ prettyShow t1 ++  ")" ++ " -> " ++ prettyShow  t2
            _ -> prettyShow t1 ++ " -> " ++ prettyShow  t2

shouldParenExpr :: Expr -> Bool
shouldParenExpr Lam{} = True
shouldParenExpr _ = False
