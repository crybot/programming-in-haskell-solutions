module ParserUtils where 

import Data.Char
import Control.Applicative

-- I slightly modified the Parser type by using Haskell's records
newtype Parser a = Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f x = Parser $ \s -> case runParser x s of
                                   [] -> []
                                   [(v, out)] -> [(f v, out)]

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = Parser $ \s -> [(x, s)]

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    fa <*> xa = Parser $ \s -> case runParser fa s of
                                    [] -> []
                                    [(f, out)] -> runParser (fmap f xa) out

instance Monad Parser where
    -- return :: a -> Parser a
    return = pure

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    mx >>= f =  Parser $ \s -> case runParser mx s of
                                    [] -> []
                                    [(v, out)] -> runParser (f v) out

instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser $ const []

    -- (<|>) :: Parser a -> Parser a -> Parser a
    ax <|> ay = Parser $ \s -> case runParser ax s of
                                    [] -> runParser ay s
                                    x@[(v, out)] -> x

item :: Parser Char
item = Parser $ \s -> case s of
                           [] -> []
                           (x:xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (x ==)

notChar :: Char -> Parser Char
notChar x = sat (x /=) 

nat :: Parser Int
nat = do
    ns <- some digit
    return $ read ns

int :: Parser Int
int = do
        char '-'
        n <- nat
        return (-n)
    <|> nat

string :: String -> Parser String
string [] = return []
string (x:xs) = do
    char x
    string xs
    return (x:xs)

space :: Parser ()
space = do
    many $ sat isSpace
    return ()

token :: Parser a -> Parser a
token parser = do
    space
    v <- parser
    space
    return v

symbol :: String -> Parser String
symbol x = token $ string x

natural ::  Parser Int
natural = token nat


--main :: IO ()
--main = print $ runParser (string "123") "123"


