{
module Parser.Lexer where

import JetErrorM
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
    $white+	;
    true                            { \p s -> TokTrue p }
    false                           { \p s -> TokFalse p }
    Bool                            { \p s -> TokTBool p }
    Int                             { \p s -> TokTInt p }
    if                              { \p s -> TokIf p }
    then                            { \p s -> TokThen p }
    else                            { \p s -> TokElse p }
    let                             { \p s -> TokLet p }
    in                              { \p s -> TokIn p }
    $digit+				            { \p s -> TokInt p (read s) }
    $alpha [$alpha $digit \_ \']*   { \p s -> TokVar p s }
    \\                              { \p s -> TokLambda p }
    \->                             { \p s -> TokArrow p }
    \.                              { \p s -> TokDot p }
    \(                              { \p s -> TokLBrace p }
    \)                              { \p s -> TokRBrace p }
    :                               { \p s -> TokColon p }
    =                               { \p s -> TokEq p }

{
data Token 
    = TokLambda AlexPosn
    | TokInt AlexPosn Int
    | TokVar AlexPosn String
    | TokTrue AlexPosn
    | TokFalse AlexPosn
    | TokArrow AlexPosn
    | TokTBool AlexPosn
    | TokTInt AlexPosn
    | TokIf AlexPosn
    | TokThen AlexPosn
    | TokElse AlexPosn
    | TokLet AlexPosn
    | TokIn AlexPosn
    | TokDot AlexPosn
    | TokLBrace AlexPosn
    | TokRBrace AlexPosn
    | TokColon AlexPosn
    | TokEq AlexPosn
    | TokEOF AlexPosn

getLoc :: Token -> (Int, Int)
getLoc (TokLambda (AlexPn _ line col)) = (line, col)
getLoc (TokInt (AlexPn _ line col) _) = (line, col)
getLoc (TokVar (AlexPn _ line col) _) = (line, col)
getLoc (TokTrue (AlexPn _ line col)) = (line, col)
getLoc (TokFalse (AlexPn _ line col)) = (line, col)
getLoc (TokArrow (AlexPn _ line col)) = (line, col)
getLoc (TokTBool (AlexPn _ line col)) = (line, col)
getLoc (TokTInt (AlexPn _ line col)) = (line, col)
getLoc (TokIf (AlexPn _ line col)) = (line, col)
getLoc (TokThen (AlexPn _ line col)) = (line, col)
getLoc (TokElse (AlexPn _ line col)) = (line, col)
getLoc (TokLet (AlexPn _ line col)) = (line, col)
getLoc (TokIn (AlexPn _ line col)) = (line, col)
getLoc (TokDot (AlexPn _ line col)) = (line, col)
getLoc (TokLBrace (AlexPn _ line col)) = (line, col)
getLoc (TokRBrace (AlexPn _ line col)) = (line, col)
getLoc (TokColon (AlexPn _ line col)) = (line, col)
getLoc (TokEq (AlexPn _ line col)) = (line, col)
getLoc (TokEOF (AlexPn _ line col)) = (line, col)

instance Show Token where
    show (TokLambda _) = "\\"
    show (TokInt _ n) = show n
    show (TokVar _ s) = s
    show (TokTrue _) = "true"
    show (TokFalse _) = "false"
    show (TokArrow _) = "->"
    show (TokTBool _) = "Bool"
    show (TokTInt _) = "Int"
    show (TokIf _) = "if"
    show (TokLet _) = "let"
    show (TokIn _) = "in"
    show (TokThen _) = "then"
    show (TokElse _) = "else"
    show (TokDot _) = "."
    show (TokLBrace _) = "("
    show (TokRBrace _) = ")"
    show (TokColon _) = ":"
    show (TokEq _) = "="
    show (TokEOF _) = "EOF"

mkLexerError :: (AlexPosn, Char, [Byte], String) -> String
mkLexerError (AlexPn _ line col, c, bl, str) = let errC = (case str of (errC:_) -> errC; [] -> c) in
    "Lexer Error " ++ show line ++ ":" ++ show col ++ " Unknown token at character \"" ++ [errC] ++ "\""

stlcLex :: String -> JetError [Token]
stlcLex s = go (alexStartPos, '\n', [], s)
    where
        go inp@(pos, _, bl, str') = 
            case alexScan inp 0 of
                AlexEOF -> return []
                AlexError inp' -> Fail (mkLexerError inp')
                AlexSkip inp' _ -> go inp'
                AlexToken inp' len act -> do
                    let tok = act pos (take len str')
                    toks <- go inp'
                    return (tok : toks)
}