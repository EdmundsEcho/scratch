import Data.List

all' p = and . map p
all'' p = not . any (not . p)
all''' p xs = foldl(&&) True (map p xs)
all'''' p xs = foldr (&&) True (map p xs)
all''''' p = foldr (&&) True . map p

any' p = or . map p
any'' p xs = length (filter p xs) > 0
any''' p = not . null . dropWhile (not . p)
any'''' p xs = not (all (\x -> not (p x)) xs)
any''''' p xs = foldr (\x acc -> (p x) || acc) False xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []
                    
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs
--45GroveSt                    
-- reverses the list - not intended                    
map' :: (a -> b) -> [a] -> [b]
--map' f = foldl (\xs x -> f x : xs) []
map' f = foldl (\xs x -> xs ++ [f x]) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p (x:xs) | p x = x : filter' p xs
                 | otherwise = filter' p xs
                 
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x xs -> if p x then x:xs else xs) []                 

dec2int :: [Integer] -> Integer
dec2int = foldl (\x y -> 10 * x + y) 0

sumsqreven = compose [map (^2), filter even]
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- we are not changing the function
-- we are just wrapping it with a different
-- way to apply the arguments. So body needs to be
-- uncurried (tuple).
curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \x y -> f (x,y)

-- here we are starting with a curried function.  So
-- the body needs to be a curried function.
uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f = \ (x,y) -> f x y

test (x,y) = x + y
test2 x y = x + y

-- unfold
-- [] if p x = True
-- else x:xs applying h x, t x -> seed that is
-- recursively processed by unfold.
type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

--int2bin' = unfoldr (==0) (`mod` 2) (`div` 2)



chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)
{-
chop8' = unfoldr null (take 8) (drop 8)

unfold :: (a -> Bool) (a -> a) (a -> a)
(a -> Bool) => when to return []
(a -> a) => is a function to process head
(a -> a) => how to reduce tail value before recursion

For map: 
when tail is [] is the time to return []
apply f to head

map' :: (a -> b) -> [a] -> [b]
map' f = unfoldr null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) id f
-}
p = (>8)
f = (2+)
xs = [3,4,7,9]
ys = [10,20,40]

t1 = (filter p . map f) xs == (map f . filter p) xs
t2 = (reverse (map f xs)) == (map f (reverse xs))
t3 = reverse (xs ++ ys) == reverse ys ++ reverse xs
t4 = filter p (map f xs) == [f x | x <- xs, p (f x)]
--t2 = filter p == filter (not . p)