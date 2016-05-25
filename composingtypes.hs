-- composing types
-- seguay to monad transformers

newtype Identity a =
    Identity { runIdentity :: a }

-- the :kind will look a lot like
-- :type for (.)    
newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)
-- Compose [Just 1, Nothing]
-- f ~ []
-- g ~ Maybe
-- a ~ Int

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)
    
-- we can implement Functor on Compose
-- if both f and g are Functors
instance (Functor f, Functor g) => 
                Functor (Compose f g) where
   fmap f (Compose fga) = Compose $ (fmap . fmap) f fga
   
-- Maybe transformer
newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }
    
instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma
    
-- the value inside the ReaderT is a function
newtype ReaderT r m a =
    ReaderT { runReaderT :: r -> m a }
    
newtype StateT s m a =
    StateT { runStateT :: s -> m (a,s) }
    