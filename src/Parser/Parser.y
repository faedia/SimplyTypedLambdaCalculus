{
module Parser.Parser where
import Parser.Lexer
import Lang.Ast
}

%name stlc
%tokentype { token }
%error { parserError }

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
    "("             { TokLBrac }
    ")"             { TokRBrac }
%%
