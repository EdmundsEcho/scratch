-- monad wadler
data Term = Con Int | Div Term Term deriving (Show)

-- accepts initial state, returns the new state
type M a = (Output,a)
type Output = String

eval :: Term -> M Int
eval (Con a)   = (line(Con a) a,a)
eval (Div t u) =
    let (x,a) = eval t
        (y,b) = eval u
    in (x ++ y ++ line (Div t u) (a`div`b), a`div`b)


  -- alternative to reverse output
        -- line (Div t u) (a`div`b) ++ o' ++ o

line :: Term -> Int -> Output
line t a = "eval(" ++ show t ++ ") <= " ++ show a ++ " \n "

answer, error' :: Term
answer = (Div(Div(Con 1972) (Con 2)) (Con 23))
error' = (Div(Con 1) (Con 0))

--eval answer
--eval error'
