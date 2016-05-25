-- monad revised
data Term = Con Int | Div Term Term deriving (Show)

eval :: Term -> Int
eval (Con a) = a
eval (Div a b) = eval a `div` eval b

answer, error' :: Term
answer = (Div(Div(Con 1972) (Con 2)) (Con 23))
error' = (Div(Con 1) (Con 0))