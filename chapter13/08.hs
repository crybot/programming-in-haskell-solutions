-- 8. Consider expressions built up from natural numbers using
-- a subtraction operator that is assumed to associate to the left.
--
-- a. Translate this descrition directly into a grammar.
-- b. Implement this grammar as a parser expr :: Parser Int.
-- c. What is the problem with this parser?
-- d. Show how it can be fixed. Hint: rewrite the parser using the
--    repetition primitive many and the library function foldl

-- Grammar:
-- expr ::= expr | expr - nat | nat | empty
-- nat  ::= 0 | 1 | 2 | ...

import ParserUtils
import Control.Applicative

-- This parser will loop indefinetely in that it will recursively expand
-- the `expr` production of the grammar before consuming any terminal
-- character
expr :: Parser Int
expr = do e <- expr
          do symbol "-"
             n <- natural
             return (e - n)
          <|>
          natural
          <|>
          empty

expr' :: Parser Int
expr' = do n <- natural
           ns <- many (do symbol "-"
                          natural)
           return $ foldl (-) n ns

