--{-# LANGUAGE InstanceSigs #-}

f :: a -> b
f = undefined

fm :: (a -> IdentityT m b)
fm = undefined

g :: b -> c
g = undefined 

a :: a
a = undefined

newtype IdentityT f a = IdentityT { runIdentityT :: f a }
        deriving (Eq, Show)
        
instance (Functor f) => Functor (IdentityT f) where
    fmap f (IdentityT fa) = IdentityT $ (fmap f) fa
    
instance (Applicative f) => Applicative (IdentityT f) where
    pure a = IdentityT (pure a)
    (IdentityT fab) <*> (IdentityT fa)  = IdentityT (fab <*> fa)
    
instance (Monad m) => Monad (IdentityT m) where
    return = pure
    (IdentityT ma) >>= f = IdentityT $ ma >>= \a -> (runIdentityT . f) a
    -- the m in ma drives which >>= is used
    -- We don't know yet what (IdentityT m) is
    -- specificially.
    
ident = IdentityT [1,3,4]