import Test.QuickCheck

foldl1 f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a

foldl2 f a bs = foldr (\a b -> f b a) a bs

foldl3 f = flip $ foldr (\a b g -> b (f g a)) id

foldl4 = foldr . flip


test_a :: String -> [String] -> Bool
test_a x xs = foldl (++) x xs == foldl2 (++) x xs

{-
If we load this into GHCi and run quickCheck test_a, it will randomly generate 100 different [x, xs] inputs to compare, fold them using the real foldl and our foldl implementation, and assert that the results are the same.
This should get you started. See the Jam Session on QuickCheck for a better explanation. I'm also new to QuickCheck, and I'd be interested in knowing if there's a more general way to test this -- I had trouble type checking f x xs without some constraints.
-}