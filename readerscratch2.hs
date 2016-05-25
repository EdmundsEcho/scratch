-- Reader 2
import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs 

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

-- have each function get parameter
-- at the same time; the partially applied
-- functions will evaluate when have access
-- to [Char] parameter
tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    a <- rev
    b <- cap
    return (a,b)
 
-- (>>=) :: m a -> (a -> m b) -> m b
-- m :: []
-- a, b :: Char
hole = undefined

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = rev >>= \a -> cap >>= \b -> return (a,b)