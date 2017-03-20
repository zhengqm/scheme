module Main where

import MyParser
import Evaluator
import LispTypes
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ fmap show $ readExpr (args !! 0) >>= eval
    putStrLn . show . extractValue $ trapError evaled
