import Data.List

-- edx countdown problem
data Op = Add | Sub | Mul | Div deriving (Show)
data Expr = Val Int | App Op Expr Expr deriving (Show)-- note one App for all Op
-- we want symbolic representation of
-- the expression; not the evalution
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- coding for the rules of the game
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y -- avoid using 2,3 and 3,2 for instance
valid Sub x y = x > y
valid Mul x y = x <= y && x /= 1 && y /= 1-- ditto
valid Div x y = x `mod` y == 0 && y /= 1

-- returns [] when fails, singleton list when success
-- benefits of list (versus Maybe) are many 
-- includig using list comprehensions
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l
                                , y <- eval r
                                , valid o x y]
-- apply only happens to valid expressions
-- subject of function is Expr

-- choices [1,2] => all lists
choices :: [a] -> [[a]]
choices [] = [[]]
choices xs = [zs | ys <- subsequences xs, zs <- permutations ys]
 --filterM (\_ -> [False,True])

-- return list of values from an expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- must evalue to n (the number in the game)
-- given expr is it a solution
-- Int = target number
-- when evaluate e must match target
-- [Int] choices
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns)
                  && eval e == [n] -- exp must evaluate to n
                  
-- [1,2,3,4] => [1] [2,3,4] [1,2],[3,4] etc                  
-- drop split with ([],full list)
split :: [a] -> [([a],[a])]
split [] = [([],[])]
split xs = tail.init $ zip (inits xs) (tails xs) 

-- This function is the heart of the solution.
-- Return a list of all possible expressions whose values
-- are precisely a given list of numbers
exprs :: [Int] -> [Expr]  -- Int matches the Value of Expr
                          -- for every Int in the list
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns
              , l       <- exprs ls
              , r       <- exprs rs
              , e       <- combine l r]
              
-- combine two expressions using each operator
-- part of the solution generator: try all operators
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add,Sub,Mul,Div]]

-- return a list of al possible expressions that solve
-- this instance of the countdown problem
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns
                    , e <- exprs ns'
                    , eval e == [n]]
                    
{-
  Problem: using brute force.  Can we combine generation
  of expressions with evaluation so we can eliminate 
  options sooner.  e.g, as soon as a combination exceeds
  our target.
  
  To do this we need to combine the generation with evaluation.

  -}
  
type Result = (Expr, Int)  -- Exptression and its value
                           -- combine symbolic with numeric 
                           -- version of expression
{-
results :: [Int] -> [Result]
results ns = [(e,n) | e <- exprs ns
                    , n <- eval e]
-}                   
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns
                  , lx      <- results ls
                  , ry      <- results rs
                  , res     <- combine' lx ry]
                  
combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) 
                        | o <- [Add,Sub,Mul,Div]
                        , valid o x y] -- check if valid with
                                       -- values carried around
                        
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns'   <- choices ns
                     , (e,m) <- results ns'
                     , m == n]
                     
--- other functions for the exercise
removeone x [] = []
removeone x (y:ys) | x == y = ys
                   | otherwise = y : removeone x ys
                   
isChoice [] _ = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeone x ys)

split' :: [a] -> [([a],[a])]
split' [] = []
split' [_] = []
split' (x:xs) = ([x],xs) : [(x:ls,rs)|(ls,rs) <- split xs]