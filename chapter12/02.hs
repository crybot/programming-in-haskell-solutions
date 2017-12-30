-- 2. Complete the following instance declaration to make the
-- partially-applied function type (a -> ) into a functor:
--  instance Functor ((->) a) where

instance Functor ((->) a) where
    --fmap :: (b -> c) -> (->) a b -> (->) a c
    fmap f g = f . g -- or fmap = (.)

-- Notes: fmap, in the above context, models the function composition
-- operator. Also, you can't run this script as the same functor is already
-- defined in 'GHC.Base'

