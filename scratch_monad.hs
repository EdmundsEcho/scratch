-- monads
{-# LANGUAGE InstanceSigs #-}

import Control.Monad (join)
-- monad as a generalized concat; flattening layers
-- (of monadic structure it injects)
-- write bind in terms of fmap and join
bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join . fmap f $ m 

-- we can do IO without monads
-- fmap putStrLn getLine

data Sum a' a  = First a'
              | Second a deriving (Eq, Show)
              
instance Functor (Sum a') where 
    fmap f (Second a) = Second (f a)
    fmap f (First a') = First a'
    
instance Applicative (Sum a') where 
    pure a  = Second a
    (<*>) :: Sum a' (a -> b) -> Sum a' a -> Sum a' b
--    Second f <*> Second a = Second f a
--    Second _ <*> First  a = First a
--    First  _ <*> Second a = First a
--    First  _ <*> First  a = First a

instance Monad (Sum a) where 
    return = pure 
    (>>=) = undefined

twiceWhenEven :: [Integer] -> [Integer] 
twiceWhenEven xs = do
    x <- xs
    if even x 
        then [x+x]  -- monad creates extra structure, but also joins..
        else [x]    -- thus giving me a single list structure.
        
data Cow = Cow { name   :: String
               , age    :: Int
               , weight :: Int  } deriving (Eq, Show)

noEmpty :: String -> Maybe String 
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int 
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow 
weightCheck c = let w = weight c 
                    n = name c
                in if n == "Bess" && w > 499 
                    then Nothing
                    else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow 
mkSphericalCow name' age' weight' =
    case noEmpty name' of 
        Nothing -> Nothing 
        Just name' ->
            case noNegative age' of 
                Nothing -> Nothing
                Just age' -> Just (Cow name' age' weight')
                
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow 
mkSphericalCow' name' age' weight' = do
    nammy <- noEmpty name'
    agey <- noNegative age'
    weighty <- noNegative weight' 
    weightCheck (Cow nammy agey weighty)
    
-- instance Monad Maybe where
--     return x = Just x 
--     (Just x) >>= k = k x
--     Nothing  >>= _ = Nothing

-- BIG: k is the rest of the do notation (even more clear when 
-- desugar do notation)


    