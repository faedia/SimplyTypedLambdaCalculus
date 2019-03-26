module Lang.Ast where

newtype Id = Id String deriving (Eq, Ord, Show, Read)

data Literal 
    = LitInt Int
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
