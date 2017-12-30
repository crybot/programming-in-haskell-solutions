-- 7. Given the following type for expressions
--  data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show
-- that contain variables of some type a, show how to make this type into
-- instances of the Functor, Applicative and Monad classes. With the aid of
-- an example. explain what the >>= operator for this type does.

data Expr a = Var a 
            | Val Int 
            | Add (Expr a) (Expr a) 
            deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap f exp = case exp of 
                      Var x -> Var (f x)
                      Val x -> Val x
                      Add e1 e2 -> Add (fmap f e1) (fmap f e2)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure = Var

    -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    Var f <*> x = fmap f x
    Add f g <*> x = Add (f <*> x) (g <*> x)

instance Monad Expr where
    -- return :: a -> Expr a
    return = pure

    -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    Val x >>= _ = Val x
    Var x >>= mf = mf x
    Add e1 e2 >>= mf = Add (e1 >>= mf) (e2 >>= mf)

-- The bind operator (>>=) works by means of simple recursion: it applies
-- the function to any Var as the base base and recursively applies itself
-- to the inner expressions of more complex ones. For example:

main :: IO ()
main = 
    let e = Add (Var 2) (Var 3) in
    print $ e >>= \n -> return (n^2)



