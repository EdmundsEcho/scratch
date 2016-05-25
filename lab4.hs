module Lab4 where

------------------------------------------------------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------

import Data.Char

-- ===================================
-- Ex. 0
-- ===================================

triangle :: Integer -> Integer
triangle n | n == 0 = 0
           | otherwise = n + triangle (n-1)

-- ===================================
-- Ex. 1
-- ===================================

count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys) | x == y = 1 + count x ys
               | otherwise = count x ys

xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]
ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]

-- ===================================
-- Ex. 2
-- ===================================

euclid :: (Int,  Int) -> Int
euclid (x, y) | x == y = x
              | x < y = euclid (x,(y-x))
              | otherwise = euclid (y,(x-y))

-- ===================================
-- Ex. 3
-- ===================================
{-
Define a recursive function funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b] that takes as arguments two functions f and g and a list xs, and applies f to all elements at even positions ([0, 2..]) in xs and g to all elements at odd positions ([1, 3..]) in xs.
Example: funkyMap (+10) (+100) [1, 2, 3, 4, 5] = [(+10) 1, (+100) 2, (+10) 3, (+100) 4, (+10) 5].
What is the result of: sum $ funkyMap (+10) (+100) ys, where ys is defined in the template?    
-}

funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMap f g xs = fmap fn (zip (concat.repeat $ [f,g]) xs)
    where fn (fg,x) = fg x