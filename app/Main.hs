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
                        1 -> evalAndPrint $ args !! 0
                        otherwise -> putStrLn "Program takes only 0 or 1 argument"

evalAndPrint :: String -> IO ()
evalAndPrint expr = putStrLn $ evalString expr

evalString :: String -> String
evalString expr = extractValue $ trapError (fmap show $ readExpr expr >>= eval)

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

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
