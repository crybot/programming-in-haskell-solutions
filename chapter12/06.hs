-- 6. Define an instance for the Monad class for the type (a ->)

instance Monad ((->) r) where
    -- return :: a -> (->) r a
    return = pure

    -- (>>=) :: (->) r a -> (a -> (->) r b) -> (->) r b
    -- (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
    mx >>= mf = \x -> mf (mx x) x



