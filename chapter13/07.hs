-- 7. Further extend the grammar and parser for arithmetic expressions to
-- support exponentiation ^, which is assumed to associate to the right and
-- have higher priority than multiplication and division, but lower priotity
-- than parentheses

-- Extended grammar:
--  expr    ::= term (+ expr | -expr | eps)
--  term    ::= power (* term | /term | eps)
--  power   ::= factor (^ power | eps)
--  factor  ::= ( expr ) | int
--  int     ::= ... | -1 | 0 | 1 | ...

import ParserUtils
import Control.Applicative

expr :: Parser Int
expr = do t <- term
          do
            op <- symbol "+" <|> symbol "-"
            e <- expr
            return (if op == "+" then t + e else t - e)
            <|> return t

term :: Parser Int
term = do p <- power
          do
            op <- symbol "*" <|> symbol "/"
            t <- term
            return (if op == "*" then p * t else p `div` t)
            <|> return p

power :: Parser Int
power = do f <- factor
           do
             symbol "^"
             p <- power
             return (f ^ p)
             <|> return f


factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            <|> integer


