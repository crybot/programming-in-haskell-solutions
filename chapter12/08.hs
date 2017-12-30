-- 8. Rather than making a parameterised type into instances of the
-- Functor, Applicative and Monad classes in this order, in practice it is
-- sometimes simpler to define the functor and applciative instances in
-- terms of the monad instance, relying on the fact that the order in which
-- declarations are made is not important in Haskell. Complete the missing
-- parts in the following declarations for the ST type using the do
-- notation.

type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do
        s <- st
        return $ g s

instance Applicative ST where
    -- pure :: a -> ST a
    pure = S . (,) -- == pure x = S (\s -> (x, s))

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do
        f <- stf
        x <- stx
        return (f x)

instance Monad ST where 
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S $ \s ->
        let (x, s') = app st s in app (f x) s'

-- Example with trees --
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

next :: ST Int
next = S $ \n -> (n, n+1)

relabel :: Tree a -> ST (Tree Int)
relabel (Leaf _) = do
    n <- next
    return $ Leaf n

relabel (Node l r) = do
    l' <- relabel l
    r' <- relabel r
    return $ Node l' r'

main :: IO ()
main = do
    let t = Node (Node (Leaf 'x') (Leaf 'y')) (Leaf 'z')
    print . fst $ app (relabel t) 0
