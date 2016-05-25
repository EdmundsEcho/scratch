{-# LANGUAGE InstanceSigs #-}

-- Monoids and Phantom Applicative functors

f :: a -> b
f = undefined

-- note the a in the type class, 
-- missing in the type value
newtype Acc o a = Acc { acc:: o }

instance Monoid (Acc o a) where
    mempty  = undefined
    mappend = undefined
    
-- note how we use promise of a to "type"
instance (Monoid o) => Functor (Acc o) where 
    fmap = undefined 
    
instance Monoid o => Applicative (Acc o) where
    pure _ = Acc mempty
    Acc o1 <*> Acc o2 = Acc $ o1 `mappend` o2