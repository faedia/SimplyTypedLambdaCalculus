module Main where

import Parser.Lexer
import Parser.Parser
import TypeCheck
import JetContext
import JetErrorM
import Lang.Ast
import Lang.Pretty
import Data.Text
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))

runCmd :: Context -> String -> JetError (Expr, Type, Context)
runCmd ctx cmd = do
    tokens <- stlcLex cmd
    tree <- stlc tokens
    t <- inferExpr tree (emptyContext :: Context)
    return (tree, t, ctx)

run :: Context -> IO ()
run ctx = do
    putStr "stlc> "
    cmd <- getLine
    let nextRun = run ctx
    case (unpack . strip . pack) cmd of
        (':':cmd) -> case cmd of
            "q" -> return ()
            _ -> putStrLn ("Unknown command: " ++ cmd) >> nextRun
        "" -> nextRun
        _ -> case runCmd ctx cmd of
            Succ (e, t, ctx') -> putStrLn (prettyShow e ++ " :: " ++ prettyShow t) >> run ctx'
            Fail err -> putStrLn err >> nextRun

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    run emptyContext
