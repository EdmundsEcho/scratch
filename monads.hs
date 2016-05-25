-- Extra reading on monad
-- list monad

instance Monad [] where
    --return :: a -> m a
    return = \x -> [x]
    --(>>=) :: m a -> (a -> m b) -> m b
    xs >>= f = concat (map f xs)

--type ST a = State -> (a, State)
-- use newType or data to allow overloading
data ST a = S (State -> (a, State)) -- use data to enable overlaoding
                                    -- note use of dummy constructor
                                    -- S for brevity

-- for convenience define our own apply
apply :: ST a -> State -> (a,State)
apply (s f) x = f x

instance Monad ST where
    --return :: a -> m a
    return x = \s -> (x,s) -- return state and x value unchanged
    st >>= f = let (x,s') = st s in f x s' 
    
instance Monad ST where
    -- return :: a -> ST a
    return x = S (\s -> (x,s))
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    s >>= f = S (\s -> let (x,s') = apply st s in apply (f x) s')
    
-- example: monad that returns current state as value, and next integer
-- as the new state