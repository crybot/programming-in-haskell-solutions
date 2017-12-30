-- 3. Define an instance of the Applicative class for the type (a-> ).


instance Functor ((->) r) where
    fmap = (.)

instance Applicative ((->) r) where
    -- pure :: a -> (->) r a
    pure = const

    -- r -> (a -> b) = r -> a -> b
    -- (<*>) :: (->) r (a -> b) -> (->) r a -> (->) r b
    fa <*> ga = \x -> fa x (ga x) 

