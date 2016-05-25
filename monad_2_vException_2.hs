-- monad take 2
-- We add a monad and introduce it
-- into the evaluator
data Term = Con Int | Div Term Term deriving (Show)

-- Monad is a trifecta
-- type constructor, unit and bind
data M a = Raise Exception | Return a deriving (Show)
type Exception = String

unit :: a -> M a
unit a = Return a

bind :: M a -> (a -> M b) -> M b
m `bind` k = case m of
               Raise e -> Raise e
               Return a -> k a

raise :: Exception -> M a
raise e = Raise e
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
                   if b == 0
                       then raise "divide by zero"
                       else unit (a `div` b)

answer, error' :: Term
answer = (Div(Div(Con 1972) (Con 2)) (Con 23))
error' = (Div(Con 1) (Con 0))

--eval answer
--eval error'