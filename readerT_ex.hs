module ReaderPractice where

import Control.Applicative 
import Data.Maybe

x=[1,2,3] 
y=[4,5,6] 
z=[7,8,9]

--lookup :: Eq a => a -> [(a,b)] -> Maybe b
--lookup = undefined

--zip x and y using 3 as the lookup key
xs :: Maybe Integer 
xs = lookup 3 (zip x y)

--zip y and z using 6 as the lookup key
ys :: Maybe Integer 
ys = lookup 6 (zip y z)


-- it's also nice to have one that
-- will return Nothing, like this one 
-- zip x and y using 4 as the look up key 
zs :: Maybe Integer 
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer 
z' n = lookup n (zip x z)

-- make tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = undefined

x2 :: Maybe (Integer, Integer)
x2 = undefined

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

--uncurry :: (a -> b -> c) -> (a,b) -> c
summed :: Num c => (c,c) -> c
summed = uncurry (+)

-- lifting summed
-- into Maybe (Integer, Integer)
-- produced using Application of pure (,)
-- lifted into to xs and ys
s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3),(<8),even] m

main :: IO () 
main = do
    print $ sequenceA [Just 3, Just 2, Just 1] 
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys) 
    print $ fmap summed ((,) <$> xs <*> zs) 
    print $ bolt 7
    print $ fmap bolt z
    print $ foldl (&&) True (sequA 4)
    print $ fmap sequA s'
    print $ fmap bolt ys
    print $ fmap bolt (z' 4)
    