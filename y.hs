module Main where

import Control.Monad (liftM)
import Data.List (foldl')
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment   (getArgs)

-- The possible LISP values
data LispVal = Atom         String
             | List         [LispVal]
             | DottedList   [LispVal] LispVal
             | Float        Float
             | Number       Integer
             | Character    Char
             | String       String
             | Bool         Bool
             deriving (Show)

-- The program (in IO)
-- execute the arguments given in parameters
main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !!0))

-- ReadExpr will take a program as input
-- and will return the result of a parseExpr
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                    Left  err -> "No match: " ++ show err
                    Right val -> showVal val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Character c) = '\'':c:'\'':[]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

-- parseExpr will parse the Expression
parseExpr :: Parser LispVal
parseExpr = parseString
        <|> try parseChar   -- #\a #\b etc...
        <|> try parseFloat  -- 3.1415
        <|> parseNumber     -- 3, #b011001, #o070, #d930, #xFF3
        <|> parseAtom       -- symbol-323
        <|> parseQuoted
        <|> do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseChar :: Parser LispVal
parseChar = do
    string "#\\"
    c <- anyChar
    return $ Character c

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

parseFloat :: Parser LispVal
parseFloat = do
    numBeforeDot <- many1 digit
    char '.'
    numAfterDot <- many1 digit
    return $ Float (read (numBeforeDot ++ "." ++ numAfterDot))

parseNumber :: Parser LispVal
parseNumber = do
    number <-     parseSimpleNumber
              <|> parseBaseSpecifiedNumber
    return (Number number)

-- Recursive Parsers

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
