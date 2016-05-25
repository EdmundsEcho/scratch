import Prelude hiding (sum, product, length)
import Control.Foldl
import Data.Monoid

data Hole = Hole
hole = undefined

sumSq :: (Num a) => Fold a a
sumSq = Fold (\x -> sum (x ^ 2)) Hole

average :: (Fractional a) => Fold a a
average = (\s c -> s / c) <$> sum <*> genericLength

std :: (Floating a) => Fold a a
std =  (\ss s len -> sqrt (ss / len - (s / len)^2))
    <$> sumSq
    <*> sum
    <*> genericLength