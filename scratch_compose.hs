-- Compose
{-# LANGUAGE InstanceSigs #-}

import Control.Monad

-- this is how we lift over a one layer structure
newtype Identity a = Identity a deriving (Eq, Ord, Show)
      
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)
    
instance Applicative Identity where 
    pure a = Identity a
    Identity f <*> Identity a = Identity (f a)
        
instance Monad Identity where
    return :: a -> Identity a
    return = pure
    
    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    Identity a >>= f = undefined
        
-- We can generalize this with one layer
newtype One f a = One (f a) 
                  deriving (Eq, Show)
                  
instance Functor f' => Functor (One f') where -- f' ~ structure
    fmap :: (a -> b) -> One f' a -> One f' b
    fmap f (One f'a) = One $ fmap f f'a

-- and three layers...
newtype Three f g h a = Three (f (g (h a))) 
                        deriving (Eq, Show)
                  
instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
    fmap :: (a -> b) -> Three f g h a -> Three f g h b
    fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha


-- more on lifting functions over wrappers
newtype Wrap f a = Wrap { getWrap :: (f a) }
                   deriving (Eq, Show)
                
instance Functor f' => Functor (Wrap f') where -- f' ~ structure
    fmap :: (a -> b) -> Wrap f' a -> Wrap f' b 
    fmap f (Wrap f'a) = Wrap (fmap f f'a) -- what fmap: fsa fmap 
    
instance Applicative f' => Applicative (Wrap f') where 
    pure :: a -> Wrap f' a
    pure a = Wrap (pure a)  -- what pure: user has to type a
    
    (<*>) :: Wrap f' (a -> b) -> Wrap f' a -> Wrap f' b
    Wrap f'f <*> Wrap f'a = Wrap (f'f <*> f'a)
    
-- f and g are type constructors
-- a is a concrete type
-- Compose::(*->*)->(*->*)->*->*
-- f ~ []  g ~ Maybe  a ~ Int
-- Compose [Just 1, Nothing]
newtype Compose f g a = Compose { getCompose :: f (g a) } 
                        deriving (Eq, Show)
-- composed uses composed fmap.fmap to lift functions
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- composed uses composed pure.pure to construct composed in App    
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a 
    pure a = Compose ((pure . pure) a)  -- this extra bracket is the reason we 
                                        -- can't express in point form remove a
                                        -- on each side

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    Compose fgf <*> Compose fga = Compose ((fmap (<*>) fgf) <*> fga)

--      let -- liftApp :: f (g (a -> b)) -> f (g a -> g b)
 --            apApF = 
 --            liftApp func = fmap (<*>) func 
             
             --apF :: f (g a -> g b)
--             apF = liftApp f -- f is fsa :: f (g (a -> b))
             
             --apApF::f(g a)->f(g b)
--             apApF = (<*>) apF 
 --            in Compose (apApF x)
    -- note generally the same as fmap
    
--    Compose f (g func) <*> Compose f (g a) = Compose f (g (func a)) 
    
-- Other Functor with * -> * -> *
-- Note how we don't apply a' to a in the data type
-- thus is different compared to previous examples.
data Two a' a = Two a' a deriving (Eq, Show)

instance Functor (Two a) where
    fmap :: (a -> b) -> Two a' a -> Two a' b
    fmap f (Two a' a) = Two a' (f a)
    
data Or  a' a = First a'
             | Second a deriving (Eq, Show)
             
instance Functor (Or a) where
    fmap :: (a -> b) -> Or a' a -> Or a' b
    fmap f (First a') = First a'
    fmap f (Second a) = Second (f a)
     
-- Use fmap to simplify if then for updating a single
-- Value in a type (here type Either, Value Right)
incIfRight :: Num a => Either e a -> Either e a 
incIfRight (Right n) = Right $ n + 1 
incIfRight (Left e) = Left e

incEither :: (Num a) => Either e a -> Either e a
incEither m = fmap (+1) m    

showIfRight :: Show a => Either e a -> Either e String 
showIfRight (Right s) = Right $ show s
showIfRight (Left e) = Left e

showEither :: (Show a) => Either e a -> Either e String
showEither m = fmap show m