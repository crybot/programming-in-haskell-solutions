-- 4. There may be more than one way to make a parameterised type into an
-- applicative functor. For example. the library Control.Applicative
-- provides and alternative 'zippy' instance for lists, in which the
-- function pure makes an infinite list of copies of its argument, and the
-- operator <*> applies each argument function to the corresponding
-- argument value at the same position.
-- Complete the following declarations that implement this idea:

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z $ map g xs

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure = Z . repeat

    -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z fs) <*> (Z xs) = Z [f x | (f, x) <- zip fs xs]
