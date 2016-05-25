--{-# LANGUAGE InstanceSigs #-}

f :: a -> b
f = undefined

g :: b -> c
g = undefined 

a :: a
a = undefined

rc :: r -> m a
rc = undefined

fm :: (a -> ReaderT e m a)
fm = undefined

at :: ReaderT r m a
at = undefined

{-
About Reader:
ReaderT stores m a.  The only way to access m a
you must provide Reader a key (r ->)

So, when we want m a from using runReaderT:  
runReaderT :: ReaderT r m a -> r -> m a
-}

-- to build a new Reader with m a use
-- ReaderT $ \r -> m a
-- instead of just Reader $
-- This is relevant for anything that changes
-- the context... Monadic function
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
-- runReaderT :: ReaderT r m a -> (r -> m a)
-- the unwrapper; the opposit of the constructor, wrapper
-- idiomatic "fold the mb value out"; its stuck in a layer

-- f does not impact context; so \r -> not relevant
-- we only use ReaderT to pattern match; not build new context
instance (Functor m) => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ (fmap.fmap) f rma
    
instance (Applicative m) => Applicative (ReaderT r m) where
    pure a  = ReaderT $ (pure.pure) a
    (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ fmap (<*>) fmab <*> rma
--  (Compose f) <*> (Compose x) = Compose $ f <*> x

-- the monadic function
-- f :: a -> ReaderT r m a
instance (Monad m) => Monad (ReaderT r m) where
    return = pure
    (ReaderT rma) >>= f = ReaderT $ \r -> do -- of m
                a <- rma r         -- patternMatch :: ReaderT -> ma
                runReaderT (f a) r