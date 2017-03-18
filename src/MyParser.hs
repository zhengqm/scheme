module MyParser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ escapedChars <|> noneOf "\"\\"
                char '"'
                return $ String x

escapedChars  :: Parser Char
escapedChars = do
                char '\\'
                x <- oneOf "\\\"tnr"
                return $ case x of
                    '\\' -> x
                    '"'  -> x
                    't'  -> '\t'
                    'n'  -> '\n'
                    'r'  -> '\r'

parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> symbol <|> digit)
                let atom = first:rest
                return $ case atom of
                    "#t" -> Bool True
                    "#f" -> Bool False
                    _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber