module Lang.Ast where

import Parser.Lexer

data Id = Id String deriving (Eq, Ord, Show, Read)

data Literal 
    = LitInt Int
    | LitBool Bool
    deriving (Eq, Ord, Show, Read)

data Expr 
    = Var AlexPosn Id
    | Lit AlexPosn Literal
    | App Expr Expr
    | Lam Id Type Expr
    deriving (Eq, Show)

data Type
    = TInt
    | TBool
    | TFun Type Type
    deriving (Eq, Ord, Show, Read)
