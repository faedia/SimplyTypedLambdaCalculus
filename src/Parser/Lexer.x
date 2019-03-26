{
module Parser.Lexer where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
    $white+	;
    true {\p s -> TokTrue}
    false {\p s -> TokFalse}
    Bool {\p s -> TokTBool}
    Int {\p s -> TokTInt}
    $digit+				{ \p s -> TokInt (read s) }
    $alpha [$alpha $digit \_ \']*		{ \p s -> TokVar s }
    \\ {\p s -> TokLambda}
    \-> {\p s -> TokArrow}
    \. {\p s -> TokDot}
    \( {\p s -> TokLBrac}
    \) {\p s -> TokRBrac}

{
data Token 
    = TokLambda
    | TokInt Int
    | TokVar String
    | TokTrue
    | TokFalse
    | TokArrow
    | TokTBool
    | TokTInt
    | TokDot
    | TokLBrac
    | TokRBrac
    deriving (Show)
}