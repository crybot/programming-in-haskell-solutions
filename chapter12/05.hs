-- 5. Work out the types for the variables in the four applicative laws

-- id :: a -> a
-- x :: f a
-- a) pure id <*> x = x :: f a


-- g :: a -> b
-- x :: a
-- b) pure (g x) = pure g <*> pure x

-- x :: f (a -> b)
-- y :: a
-- g :: a -> b
-- (\g -> g y) :: (a -> b) -> b
-- c) x <*> pure y = pure (\g -> g y) <*> x


-- y :: f (a -> b)
-- z :: f a
-- y <*> z :: f b
-- x :: f (b -> c)
-- d) x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

