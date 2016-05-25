--{-# LANGUAGE InstanceSigs #-}

f :: a -> b
f = undefined

g :: b -> c
g = undefined 

a :: a
a = undefined

fm :: (a -> EitherT e m a)
fm = undefined

at :: EitherT e m a
at = undefined

-- data Either a b = Left a | Right b

-- (* -> *) not (* -> * -> *) like Compose
-- because Maybe type is concrete
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) } 
-- runEitherT :: EitherT m e a -> m (Either e a)
-- the unwrapper; the opposit of the constructor, wrapper
-- idiomatic "fold the mb value out"; its stuck in a layer

instance (Functor m) => Functor (EitherT e m) where
    fmap f (EitherT ma) = EitherT $ (fmap.fmap) f ma
    
instance (Applicative m) => Applicative (EitherT e m) where
    pure a  = EitherT $ (pure.pure) a
    (EitherT f) <*> (EitherT ma) = EitherT $ fmap (<*>) f <*> ma
--  (Compose f) <*> (Compose x) = Compose $ f <*> x

-- the monadic function
-- f :: a -> EitherT e m b (type value = m Either)
instance (Monad m) => Monad (EitherT e m) where
    return = pure
    (EitherT ma) >>= f = EitherT $ do -- of m
        v <- ma
        case v of -- patternMatch:: ma -> Either
            Left e  -> return (Left e) -- propogate the prior err
            Right a -> runEitherT (f a)
            
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e 
swapEitherT (EitherT mea) = undefined