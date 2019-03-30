module Main where

import Parser.Lexer
import Parser.Parser
import TypeCheck
import JetContext
import JetErrorM
import Lang.Ast
import Lang.Eval
import Lang.Pretty
import Data.Text
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))

frontEnd :: Context -> String -> JetError (Expr, Type)
frontEnd ctx cmd = do
    tokens <- stlcLex cmd
    tree <- stlc tokens
    t <- inferExpr tree ctx
    return (tree, t)

runCmd :: Context -> EvalContext -> String -> JetError (Literal, Context, EvalContext)
runCmd ctx ectx cmd = do
    (tree, _) <- frontEnd ctx cmd
    expr <- evalExpr ectx tree
    lit <- getValue expr
    return (lit, ctx, ectx)

run :: Context -> EvalContext -> IO ()
run ctx ectx = do
    putStr "stlc> "
    cmd <- getLine
    let nextRun = run ctx ectx
    case (unpack . strip . pack) cmd of
        (':':cmd) -> case cmd of
            "q" -> return ()
            ('t':expr) -> case frontEnd ctx expr of
                Succ (e, t) -> putStrLn (prettyShow e ++ " :: " ++ prettyShow t) >> nextRun
                Fail err -> putStrLn err >> nextRun
            _ -> putStrLn ("Unknown command: " ++ cmd) >> nextRun
        "" -> nextRun
        _ -> case runCmd ctx ectx cmd of
            Succ (lit, ctx', ectx') -> putStrLn (prettyShow lit) >> run ctx' ectx'
            Fail err -> putStrLn err >> nextRun

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    run emptyContext emptyContext
