module Lang.Ast where

import Parser.Lexer

data Id = Id String deriving (Eq, Ord, Show, Read)

data Literal 
    = LitInt Integer
    | LitBool Bool
    | LitUnit
    deriving (Eq, Ord, Show, Read)

data Expr 
    = Var AlexPosn Id
    | Lit AlexPosn Literal
    | App Expr Expr
    | Lam Id Type Expr
    | Let Id Expr Expr
    | If Expr Expr Expr
    | Fix Expr
    | Pred Expr
    | ESucc Expr
    | Iszero Expr
    deriving (Eq, Show)

data Type
    = TInt
    | TBool
    | TUnit
    | TFun Type Type
    deriving (Eq, Ord, Show, Read)
