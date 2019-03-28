module Main where

import Parser.Lexer
import Parser.Parser
import TypeCheck
import JetContext
import JetErrorM
import Lang.Ast
import Lang.Pretty
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))

runCmd :: String -> JetError (Expr, Type)
runCmd cmd = do
    tokens <- stlcLex cmd
    tree <- stlc tokens
    t <- inferExpr tree (emptyContext :: Context)
    return (tree, t)

run :: IO ()
run = do
    putStr "λ> "
    cmd <- getLine
    case cmd of
        (':':cmd) -> case cmd of
            "q" -> return ()
            _ -> putStrLn ("Unknown command: " ++ cmd) >> run
        _ -> case runCmd cmd of
            Succ (e, t) -> putStrLn (prettyShow e ++ " :: " ++ prettyShow t) >> run
            Fail err -> putStrLn err >> run

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    run
