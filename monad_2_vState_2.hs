-- monad take 2
-- we add a monad
-- and update the evaluator with unit
data Term = Con Int | Div Term Term deriving (Show)

-- Monad is a trifecta
-- type constructor, unit and bind
type M a = State -> (a,State)
type State = Int

unit :: a -> M a
unit a = \s -> (a,s)

bind :: M a -> (a -> M b) -> M b
m `bind` k = \s -> let (a,s')  = m s
                       (b,s'') = k a s' 
                   in ( b,s'')
                   
tick :: M ()
tick = \s -> ((), s + 1)


{- m * \a.n
   let a = m in n
   perform the computation m, bind it to a
   THEN perform computation n and 
   THEN return the result.
-}

eval :: Term -> M Int
eval (Con a) = unit a
eval (Div t u) = eval t `bind` \a -> 
                 eval u `bind` \b ->
                 unit (a `div` b)

answer, error' :: Term
answer = (Div(Div(Con 1972) (Con 2)) (Con 23))
error' = (Div(Con 1) (Con 0))

--eval answer 0
--eval error' 0