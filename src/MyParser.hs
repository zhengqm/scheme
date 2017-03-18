module MyParser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

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


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

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

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseListCombined :: Parser LispVal
parseListCombined = do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseListCombined












