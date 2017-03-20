module Main where

import MyParser
import Evaluator
import LispTypes
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    case length args of 0 -> runRepl
                        1 -> runOne $ args !! 0
                        otherwise -> putStrLn "Program takes only 0 or 1 argument"

runOne :: String -> IO ()
runOne expr = do
        init <- nullEnv
        evalAndPrint init expr

runRepl :: IO ()
runRepl = do
    env <- nullEnv
    until_ (== "quit") (readPrompt "Lisp>>> ") (evalAndPrint env)


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ do
    lispVal <- liftThrows $ readExpr expr
    eval env lispVal




until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout
