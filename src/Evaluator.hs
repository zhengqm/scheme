module Evaluator where

import LispTypes

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func 


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = case lookup func primitives of 
                    Just f  -> f args
                    Nothing -> Left $ NotFunction "Unrecognized primitive function args" func

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("symbol?", unaryOp symbolp),
    ("string?", unaryOp stringp),
    ("number?", unaryOp numberp),
    ("bool?", unaryOp boolp),
    ("list?", unaryOp listp)]

numericBinop :: (Integer->Integer->Integer)->[LispVal]-> ThrowsError LispVal
numericBinop op [] = Left $ NumArgs 2 []
numericBinop op singleVal@[_] = Left $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v
unaryOp f l = Left $ NumArgs 1 l

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                            if null parsed
                                then Left $ TypeMismatch "number" $ String n
                                else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum other = Left $ TypeMismatch "number" other


symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False