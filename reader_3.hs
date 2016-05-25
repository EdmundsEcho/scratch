{-# LANGUAGE FlexibleContexts #-}

-- More on reader

newtype Reader e a = Reader { runReader :: (e -> a) } 

data Either' e a = Left' e | Right' a 
     deriving (Eq, Ord, Read, Show)

data Range = TooSmall | Zero | One | Two | Three | TooBig
             deriving (Eq, Ord, Show, Read, Bounded)

instance Functor (Either' e) where
    fmap f (Right' a) = Right' (f a) 
    
instance Applicative (Either' e) where
    pure a = Right' a
    Left' e <*> _ = Left' e
    _ <*> Left' e = Left' e 
    Right' f <*> Right' x = Right' (f x)

-- the challenge with Monad is less about
-- binding, more about coding the monadic function
-- a -> m b ; that uses the binding function (is an arg)
instance Monad (Either' e) where
    return a = Right' a
    Left' e >>= _ = Left' e
    Right' a >>= f = f a
    
    
--ask r = id (runReader r)

pm :: [a] -> (a,b)
pm = undefined

list = [1,2,3]

comp :: Integer -> Integer
comp = ((+) 4).((-) 3)

comp' :: Integer -> Integer
comp' = \x -> (+) 4 ((-) 3 x)

-- This is a monadic function
range :: Int -> Either' String Range
range n = case n of
    0 -> Right' Zero
    1 -> Right' One
    2 -> Right' Two
    3 -> Right' Three
    otherwise -> Left' ("Out of range: " ++ show n)
    
toInt :: Range -> Either' String Int
toInt range = case range of
    Zero  -> Right' 0
    One   -> Right' 1
    Two   -> Right' 2
    Three -> Right' 3
    otherwise -> Left' ("Out of range: " ++ show range)
    
-- I want a function that will increment
-- my Range by one.

-- instance Functor (Reader e) where
--    fmap f r = Reader $ \e -> f _
    
--instance Monad (Reader e) where
--    return a = Reader $ \e -> a
--    (Reader r) >>= f = Reader $ \e -> f (r e) e