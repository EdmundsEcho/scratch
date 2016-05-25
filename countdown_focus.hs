import Debug.Trace

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Show)
            
repeatTree :: a -> Tree a
repeatTree x = Node t x t
    where t = repeatTree x
    
-- NEW: notice use of [_] to eliminate empty list
-- Design: Notice how split xs is used without
-- any understanding of where it will actually split the list
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

fibs = 0 : 1 : [x + y | (x,y) <- zip fibs (tail fibs)]

fib n = fibs !! n
--fib n = last (take n fibs)