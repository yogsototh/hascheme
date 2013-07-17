module Main where

import Data.List (foldl')
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment   (getArgs)

data LispVal = Atom         String
             | List         [LispVal]
             | DottedList   [LispVal] LispVal
             | Number       Integer
             | String       String
             | Bool         Bool
             deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ strSpecialChar <|> noneOf "\""
    char '"'
    return $ String x
    where
        strSpecialChar = char '\\' >> do
            x <- anyChar
            case x of
                'n' -> return '\n'
                't' -> return '\t'
                'r' -> return '\r'
                _   -> return x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
                "#vrai" -> Bool True
                "#faux" -> Bool False
                _       -> Atom atom

numFromBase n str = foldl' traiteNombre 0 str
    where
        traiteNombre acc v = acc*n + chiffre
            where chiffre = case v of
                            '0' -> 0
                            '1' -> 1
                            '2' -> 2
                            '3' -> 3
                            '4' -> 4
                            '5' -> 5
                            '6' -> 6
                            '7' -> 7
                            '8' -> 8
                            '9' -> 9
                            'A' -> 10
                            'B' -> 11
                            'C' -> 12
                            'D' -> 13
                            'E' -> 14
                            'F' -> 15
                            'a' -> 10
                            'b' -> 11
                            'c' -> 12
                            'd' -> 13
                            'e' -> 14
                            'f' -> 15

parseBaseSpecifiedNumber :: Parser Integer
parseBaseSpecifiedNumber = do
    _ <- char '#'
    numtype <- oneOf "bdox"
    (base,str) <- case numtype of
                    'b' -> do
                        numstr <- many1 (oneOf "01")
                        return (2,numstr)
                    'o' -> do
                        numstr <- many1 (oneOf "01234567")
                        return (8,numstr)
                    'd' -> do
                        numstr <- many1 (oneOf "0123456789")
                        return (10,numstr)
                    'x' -> do
                        numstr <- many1 (oneOf "0123456789ABCDEFabcdef")
                        return (16,numstr)
    return $ numFromBase base str

parseSimpleNumber :: Parser Integer
parseSimpleNumber = do
    numStr <- many1 digit
    return (read numStr)

parseNumber :: Parser LispVal
parseNumber = do
    number <-     parseSimpleNumber
              <|> parseBaseSpecifiedNumber
    return (Number number)

parseExpr :: Parser LispVal
parseExpr = parseString
        <|> parseNumber
        <|> parseAtom


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                    Left  err -> "No match: " ++ show err
                    Right val -> show val

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !!0))
