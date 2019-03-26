{
module Parser.Parser where
import Parser.Lexer
import Lang.Ast
}

%name stlc
%tokentype { Token }
%error { parseError }

%token
    true            { TokTrue }
    false           { TokFalse }
    Bool            { TokTBool }
    Int             { TokTInt }
    int_literal	    { TokInt $$ }
    var		        { TokVar $$ }
    "\\"            { TokLambda }
    "->"            { TokArrow }
    "."             { TokDot }
    "("             { TokLBrace }
    ")"             { TokRBrace }
    ":"             { TokColon }
%%

Expr    : var                           { Var (Id $1) }
        | int_literal                   { Lit (LitInt $1) }
        | true                          { Lit (LitBool True) }
        | false                         { Lit (LitBool False) }
        | Expr Expr                     { App $1 $2 }
        | "\\" var ":" Type "." Expr    { Lam (Id $2) $4 $6 }
        | "(" Expr ")"                  { $2 }

Type    : Int { TInt }
        | Bool { TBool }
        | Type "->" Type { TFun $1 $3 }
        | "(" Type ")"      { $2 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
