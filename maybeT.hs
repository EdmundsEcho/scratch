--{-# LANGUAGE InstanceSigs #-}

f :: a -> b
f = undefined

g :: b -> c
g = undefined 

a :: a
a = undefined

fm :: (a -> MaybeT m b)
fm = undefined

-- (* -> *) not (* -> * -> *) like Compose
-- because Maybe type is concrete
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) } 
-- runMaybeT :: MaybeT m a -> m (Maybe a)
-- the unwrapper; the opposit of the constructor, wrapper
-- idiomatic "fold the mb value out"; its stuck in a layer

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT mma) = MaybeT $ (fmap.fmap) f mma
    
instance (Applicative m) => Applicative (MaybeT m) where
    pure a  = MaybeT $ (pure.pure) a
    (MaybeT f) <*> (MaybeT mma) = MaybeT $ fmap (<*>) f <*> mma
--  (Compose f) <*> (Compose x) = Compose $ f <*> x

-- f :: a -> MaybeT m b
-- the monadic fumction
instance (Monad m) => Monad (MaybeT m) where
    return = pure
    (MaybeT ma) >>= f = MaybeT $ do
        v <- ma
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)