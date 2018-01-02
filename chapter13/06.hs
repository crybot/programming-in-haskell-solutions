-- 6. Extend the parser expr :: Parser Int to support subtraction and
-- division, and to use integer values rather than natural numbers, based
-- upon the following revisions to the grammar:
--  expr    ::= term (+ expr | -expr | eps)
--  term    ::= factor (* term | /term | eps)
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
term = do f <- factor
          do
            op <- symbol "*" <|> symbol "/"
            t <- term
            return (if op == "*" then f * t else f `div` t)
            <|> return f


factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            <|> integer


