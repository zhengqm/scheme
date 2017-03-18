module Main where

import MyParser
import Evaluator
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
