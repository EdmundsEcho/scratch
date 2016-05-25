--{-# LANGUAGE InstanceSigs #-}

f :: a -> b
f = undefined

g :: b -> c
g = undefined 

a :: a
a = undefined

newtype Compose f g a =
    Compose { getComp :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga
    
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure a  = Compose $ (pure.pure) a
    (Compose f) <*> (Compose x) = Compose $ fmap (<*>) f <*> x
--  (Compose f) <*> (Compose x) = Compose $            f <*> x

instance Monoid (Compose f g a) where
    mappend = undefined
    mempty  = undefined
    
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
--  foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap f (Compose fga) = (foldMap.foldMap) f fga
    
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose fga) = Compose <$> (traverse.traverse) f fga

-- and for 3 deep
-- NOTE: we could instead nest the use of Compose
-- e.g., Compose f (Compose g (h a))
newtype ThreeComp f g h a =
    ThreeComp { getThreeComp :: f (g (h a)) } deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (ThreeComp f g h) where
    fmap f (ThreeComp fgha) = ThreeComp $ (fmap.fmap.fmap) f fgha
    
instance (Applicative f, Applicative g, Applicative h) => Applicative (ThreeComp f g h) where
    pure a  = ThreeComp $ (pure.pure.pure) a
    (ThreeComp f) <*> (ThreeComp x) = ThreeComp $
                      (fmap (<*>)) ((fmap.fmap) (<*>) f) <*> x
    
x   = [(Just 4),(Just 8)]
ca  = Compose $ [(Just 4),(Just 8)]
cf1 = Compose $ [(Just (+2))]
cf2 = Compose $ [Just (+2), Just (8+)] 
cff = Compose $ [(Just f)]
caa = Compose $ [(Just a)]

c3a = ThreeComp $ Just [(Just 4),(Just 8)]
c3f = ThreeComp $ Just [(Just (+2))]

