module Main where

import MyParser
import System.Environment

main :: IO ()
main = do
        (expr:_) <- getArgs
        putStrLn (readExpr expr)
