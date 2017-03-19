module LispTypes where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import Control.Monad.Error.Class

data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool


showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List contents) = "(" ++ showLispList contents ++ ")"
showVal (DottedList head tail) = "(" ++ showLispList head ++ " . " ++ showVal tail ++ ")"
showVal (Number num) = show num
showVal (String str) = "\"" ++ str ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

showLispList :: [LispVal] -> String
showLispList = unwords . map showVal

instance Show LispVal where
    show = showVal




data LispError =  NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ (unwords $ map show found)
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError


type ThrowsError = Either LispError 

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

