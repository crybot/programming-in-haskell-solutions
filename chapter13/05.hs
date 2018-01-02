-- Define a suitable type Expr for arithmetic expressions and modify the
-- parser for expressions to have type expr :: Parser Expr

import ParserUtils
import Control.Applicative

data Expr = Val Int 
          | Add Expr Expr 
          | Prod Expr Expr
          deriving Show

expr :: Parser Expr
expr = do
    t <- term
    do
        symbol "+"
        e <- expr
        return $ Add t e
        <|> return t

term :: Parser Expr
term = do
    f <- factor
    do
        symbol "*"
        t <- term
        return $ Prod f t
        <|> return f


factor :: Parser Expr
factor = do
    symbol "("
    e <- expr
    symbol ")"
    return e
    <|> fmap Val natural


