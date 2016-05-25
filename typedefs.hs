-- edx week 9
-- declaring types
--module Main (main) where
import Data.List
import Data.Char
import Hugs.IOExts (unsafeCoerce)
import Data.Foldable (fold)
        
data Expr = Add Expr Expr 
          | Val Int
          deriving (Show,Read,Eq)
      
data Nat = Zero 
         | Succ Nat 
         deriving (Show,Read,Eq)

{-     
data Ordering = LT
              | EQ
              | GT
-}

data Tree = Leaf Integer
          | Node Tree Integer Tree
          deriving (Show,Read,Eq)
      
data Tree' = Leaf' Integer
           | Node' Tree' Tree'
           deriving (Show,Read,Eq)

natToInteger :: Nat -> Integer
natToInteger = head . m
    where m Zero = [0] -- unwraps the list monad
          m (Succ n) = [sum [x | x <- (1:m n)]]
--natToInteger = \n -> genericLength [c | c <- show n, c == 'S']
--xnatToInteger = \n -> length [c | c <- show n, c == 'S']
--natToInteger (Succ n) = natToInteger n + 1
--natToInteger Zero = 0

integerToNat :: Integer -> Nat
integerToNat (n+1) = let m = integerToNat n in Succ m
integerToNat 0 = Zero
--xintegerToNat = \n -> genericLength [c | c <- show n, isDigit c]
--integerToNat = head . m
--    where {
--          ; m 0 = [0]
--          ; m (n + 1) = [sum [x | x <- (1 : m n)]]
--          }
--xintegerToNat n = product [(unsafeCoerce c) :: Integer | c <- show n]
--integerToNat 0 = Zero
--integerToNat (n+1) = Succ (integerToNat n)

add :: Nat -> Nat -> Nat
add Zero n = n
add m Zero = m
add n (Succ m) = Succ (add m n)
--xadd n (Succ m) = Succ (add n m) -- need to alternate

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

--compare :: (Ord a) => a -> a -> Ordering
occurs :: Integer -> Tree -> Bool
--occurs m (Leaf n) = m == n
--occurs m (Node l n r) | m == n = True
--                      | m < n = occurs m l
--                      | m > n = occurs m r

occurs m (Leaf n) = m == n
occurs m (Node l n r) = case compare m n of
    LT -> occurs m l
    EQ -> True
    LT -> occurs m r

balanced :: Tree' -> Bool
leaves (Leaf' _) = 1
leaves (Node' l r) = leaves l + leaves r
balanced (Leaf' _) = True
balanced (Node' l r) = abs (leaves l - leaves r) <= 1
                     && balanced l && balanced r
                 
balance :: [Integer] -> Tree'

halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf' x
balance xs = Node' (balance ys) (balance zs)
    where (ys, zs) = halve xs

mTree = Node (Node (Leaf 2) 3 (Leaf 4))
             5 
             (Node (Leaf 6) 7 (Leaf 9))
         
mTree' = Node' (Node' (Leaf' 2) (Leaf' 4))
               (Node' (Leaf' 6) (Node' (Leaf' 7) (Leaf' 10)))
           
mTree'' = balance [6,4,8,2,90]

mempty = []
(<>) = (++)
fold = foldr (<>) mempty
        