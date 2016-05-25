-- monad wadler
data Term = Con Int | Div Term Term deriving (Show)

-- accepts initial state, returns the new state
type M a = State -> (a,State)
type State = Int

unit :: a -> M a
unit a = \x -> (a,x)

-- bind
bind :: M a -> (a -> M b) -> M b
m `bind` k = \x -> let (a,y) = m x
                       (b,z) = k a y
                   in  (b,z)

-- tick, function to move state forward
tick :: M ()
tick = \x -> ((),x+1)


eval :: Term -> M Int
eval (Con a)   = unit a
eval (Div t u) = eval t `bind` \a ->
                 eval u `bind` \b ->
                 tick `bind` \() -> unit (a`div`b)

answer, error' :: Term
answer = (Div(Div(Con 1972) (Con 2)) (Con 23))
error' = (Div(Con 1) (Con 0))

--eval answer 0
--eval error' 0
