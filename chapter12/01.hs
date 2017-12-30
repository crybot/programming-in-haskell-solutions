-- 1. Define an instance of the Functor class for the following type of
-- binary trees that have data in ther nodes:
--  data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show


data Tree a = Leaf | Node (Tree a) a (Tree a)
            deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Leaf = Leaf
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)





