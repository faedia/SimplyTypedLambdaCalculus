{
module Parser.Parser where
import Parser.Lexer
import Lang.Ast
import JetErrorM
}

%name stlc
%tokentype { Token }
%monad { JetError } { (>>=) } { return }
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
%right "->"
%%

Expr    : "\\" var ":" Type "." Expr    { Lam (Id $2) $4 $6 }
        | AppExpr                       { $1 }

AppExpr : AppExpr AppRhs                { App $1 $2 }
        | AppRhs                        { $1 }

AppRhs  : var                           { Var (Id $1) }
        | int_literal                   { Lit (LitInt $1) }
        | true                          { Lit (LitBool True) }
        | false                         { Lit (LitBool False) }
        | "(" Expr ")"                  { $2 }

Type    : Int               { TInt }
        | Bool              { TBool }
        | Type "->" Type    { TFun $1 $3 }
        | "(" Type ")"      { $2 }


{
parseError :: [Token] -> JetError a
parseError _ = Fail "Parse error"
}
