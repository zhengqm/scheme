module LispTypes where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import Data.IORef
import System.IO

data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool
                | IOFunc ([LispVal] -> IOThrowsError LispVal)
                | Port Handle
                | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
                | Func { params :: [String], vararg :: (Maybe String),
                        body :: [LispVal], closure :: Env }



showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List contents) = "(" ++ showLispList contents ++ ")"
showVal (DottedList head tail) = "(" ++ showLispList head ++ " . " ++ showVal tail ++ ")"
showVal (Number num) = show num
showVal (String str) = "\"" ++ str ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

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

type Env = IORef [(String, IORef LispVal)]

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue


isBound :: Env -> String -> IO Bool
isBound envRef var = do
    env <- readIORef envRef
    return $ case lookup var env of
        (Just _) -> True
        Nothing  -> False

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- lift $ readIORef envRef
                       case lookup var env of
                            Just ref -> lift $ readIORef ref
                            Nothing    -> throwError $ UnboundVar "Getting an unbound variable" var

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- lift $ readIORef envRef
                             case lookup var env of
                                Just ref -> lift $ writeIORef ref value
                                Nothing  -> throwError $ UnboundVar "Setting an unbound variable" var
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- lift $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else lift $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where   extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
            addBinding (var, value) = do ref <- newIORef value
                                         return (var, ref)

-- Can be used for both Either LispError / ExceptT LispError IO 
trapError action = catchError  action        (return . show)
--                           [m String]  [String -> m String].[e -> String] 

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = undefined

