-- monad revised
data Term = Con Int | Div Term Term deriving (Show)

data M a = Raise Exception | Return a deriving (Show)
type Exception = String

eval :: Term -> M Int
eval (Con a) = Return a
eval (Div t u) = case eval t of
    Raise e -> Raise e
    Return a -> 
        case eval u of
            Raise e -> Raise e
            Return b ->
              if b == 0
                  then Raise "divide by zero"
                  else Return (a `div` b)

answer, error' :: Term
answer = (Div(Div(Con 1972) (Con 2)) (Con 23))
error' = (Div(Con 1) (Con 0))