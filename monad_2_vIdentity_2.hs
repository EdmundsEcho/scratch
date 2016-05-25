-- monad revised
data Term = Con Int | Div Term Term deriving (Show)

-- Monad is a trifecta
type M a = a

unit :: a -> M a
unit a = a

bind :: M a -> (a -> M b) -> M b
a `bind` k = k a

{- m * \a.n
   let a = m in n
   perform the computation m, bind it to a
   THEN perform computation n and 
   THEN return the result.
-}

eval :: Term -> M Int
eval (Con a)   = unit a
eval (Div t u) = eval t `bind` \a -> 
                 eval u `bind` \b -> 
                 unit (a`div`b)

answer, error' :: Term
answer = (Div(Div(Con 1972) (Con 2)) (Con 23))
error' = (Div(Con 1) (Con 0))

--eval answer
--eval error'