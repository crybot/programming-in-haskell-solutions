-- 1. Define a parser comment :: Parser () for ordinary Haskell comments
-- that begin with the symbol -- and extend to the end of the current line,
-- which is represented by the control character '\n'
import ParserUtils
import Control.Applicative

comment :: Parser ()
comment = token $ do
    string "--"
    many $ notChar '\n'
    return ()

main :: IO ()
main = do
    str <- fmap (++"\n") getLine
    print $ runParser comment str

