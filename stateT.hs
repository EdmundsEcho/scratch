{-# LANGUAGE InstanceSigs #-}

f :: a -> b
f = undefined

fm :: a -> StateT s m a
fm = undefined

ma :: StateT s m a
ma = undefined

stateA = StateT $ \s -> Just (100, s)
stateF = StateT $ \s -> Just ((5+), s)

-- StateT requires s inside StateT to produce
-- anything. This is unlike ReaderT that just
-- takes input from user via \r ->

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
-- runStateT :: StateT s m a -> s -> m (a,s)

instance (Functor m) => Functor (StateT s m) where
    fmap f (StateT sma) = StateT $ \s -> [(s', f a) | (s', a) <- sma s]
--    fmap f sma = StateT $ \s -> 
  --          fmap ( \(a,_) -> (f a, s) ) (runStateT sma s) 

instance (Monad m) => Applicative (StateT s m) where
    pure a  = StateT $ \s -> pure (a, s)
    smf <*> sma = StateT $ \s -> do
            (a,_) <- (runStateT sma s)
            (f,_) <- (runStateT smf s)
            return (f a, s)
        
-- the monadic function
-- f :: a -> StateT s m a
-- f is wrong because I need (a -> m b)
-- state remaking function
instance (Monad m) => Monad (StateT s m) where
    return = pure
    sma >>= f = StateT $ \s -> do
            (a,s') <- (runStateT sma s)
            (runStateT (f a) s')