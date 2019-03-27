{
module Parser.Lexer where

import JetErrorM
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
    $white+	;
    true                            { \p s -> TokTrue }
    false                           { \p s -> TokFalse }
    Bool                            { \p s -> TokTBool }
    Int                             { \p s -> TokTInt }
    $digit+				            { \p s -> TokInt (read s) }
    $alpha [$alpha $digit \_ \']*   { \p s -> TokVar s }
    \\                              { \p s -> TokLambda }
    \->                             { \p s -> TokArrow }
    \.                              { \p s -> TokDot }
    \(                              { \p s -> TokLBrace }
    \)                              { \p s -> TokRBrace }
    :                               { \p s -> TokColon }

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
    | TokLBrace
    | TokRBrace
    | TokColon
    deriving (Show)

stlcLex :: String -> JetError [Token]
stlcLex s = go (alexStartPos, '\n', [], s)
    where
        go inp@(pos, _, bl, str') = 
            case alexScan inp 0 of
                AlexEOF -> return []
                AlexError inp' -> Fail "Lex Error"
                AlexSkip inp' _ -> go inp'
                AlexToken inp' len act -> do
                    let tok = act pos (take len str')
                    toks <- go inp'
                    return (tok : toks)
}