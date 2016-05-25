-- monad wadler
data Term = Con Int | Div Term Term deriving (Show)

-- accepts initial state, returns the new state
type M a = State -> (a,State)
type State = Int

eval :: Term -> M Int
eval (Con a) = \s -> (a,s)
eval (Div t u) = \s -> let (a,s')  = eval t s
                           (b,s'') = eval u s'
                       in (a`div`b, s''+1)

answer, error' :: Term
answer = (Div(Div(Con 1972) (Con 2)) (Con 23))
error' = (Div(Con 1) (Con 0))

--eval answer 0
--eval error' 0