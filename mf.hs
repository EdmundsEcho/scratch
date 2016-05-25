-- Poor Mans Monad
type C a = (\a -> Action) -> Action
type C m a = (\a -> Action m) -> Action m

instance Monad m => Monad (C m) where
    f >>= k = (\c -> f (\a -> k a c))
    return x = (\c -> c x)

data Action m = Atom (m (Action m))
              | Fork (Action m) (Action m)
              | Stop

action :: Monad m => C m a -> Action m
action m = m (\a -> Stop)

atom :: Monad m => m a -> C m a
atom m = (\c -> Atom (do a <- m 
                        return (c a)))

stop :: Monad m => C m a
stop = (\c -> Stop)

par :: Monad m => C m a -> C m a -> C m a
par m1 m2 = (\c -> Fork (m1 c) (m2 c))

fork :: Monad m => C m a -> C m ()
fork m = (\c -> Fork (action m) (c ()))

instance MonadTrans C where
    lift = atom

round :: Monad m => [Action m] -> m ()
round [] = return ()
round (a :as) = case a of
    Atom am     -> do a' <- am 
                      round (as ++ [a'])
    Fork a1 a2  -> round (as ++ [a1, a2])
    Stop        -> round as

run :: Monad m => C m a -> m ()
run m = round [action m]
     
instance Writer m => Writer (C m) where
     write s = lift (write s)                              
     
loop :: Writer m => String -> m ()
loop s = do write s
            loop s

example :: Writer m => C m ()
example = do write "start!"
             fork (loop "fish")
             loop "cat"
     
     
mf y | y < 10    = Just (y + 10)
     | otherwise = Nothing