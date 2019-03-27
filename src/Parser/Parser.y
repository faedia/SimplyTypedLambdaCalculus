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
--%lexer { stlcLex } { TokEOF _ }

%token
    true            { TokTrue $$ }
    false           { TokFalse $$ }
    Bool            { TokTBool _ }
    Int             { TokTInt _ }
    int_literal	    { TokInt pos $$ }
    var		        { TokVar pos $$ }
    "\\"            { TokLambda _ }
    "->"            { TokArrow _ }
    "."             { TokDot _ }
    "("             { TokLBrace _ }
    ")"             { TokRBrace _ }
    ":"             { TokColon _ }
%right "->"
%%

Expr    : "\\" var ":" Type "." Expr    { Lam (Id $2) $4 $6 }
        | AppExpr                       { $1 }

AppExpr : AppExpr AppRhs                { App $1 $2 }
        | AppRhs                        { $1 }

AppRhs  : var                           { Var pos (Id $1) }
        | int_literal                   { Lit pos (LitInt $1) }
        | true                          { Lit $1 (LitBool True) }
        | false                         { Lit $1 (LitBool False) }
        | "(" Expr ")"                  { $2 }

Type    : Int               { TInt }
        | Bool              { TBool }
        | Type "->" Type    { TFun $1 $3 }
        | "(" Type ")"      { $2 }


{
parseError :: [Token] -> JetError a
parseError [tok] = let loc = getLoc tok in
    Fail ("Parse Error " ++ show (fst loc) ++ ":" ++ show (snd loc) ++ " found unexpected \"" ++ show tok ++ "\"")
parseError toks = Fail ("Unexpected end of input")
}
