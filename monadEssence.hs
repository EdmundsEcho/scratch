-- monad

type Name           = String

data Term           = Var Name
                    | Con Int
                    | Add Term Term
                    | Lam Name Term
                    | App Term Term
                    deriving (Show)
                    
data Value          = Wrong
                    | Num Int
                    | Fun (Value -> Identity Value)
                    
type Environment = [(Name, Value)]

type Identity a = a

unitId a = a
a `bindId` k = k a
showId a = showval a

unitId              :: a -> Identity a
bindId              :: Identity a -> (a -> Identity b) -> (Identity b)

showval             :: Value -> String
showval Wrong       = "<wrong>"
showval (Num i)     = show i
showval (Fun f)     = "<function>"

eval                :: Term -> Environment -> Identity Value
eval (Var x) e      = lookup' x e
eval (Con i) e      = unitId (Num i)
eval (Add u v) e    = eval u e `bindId` (\a ->
                      eval v e `bindId` (\b ->
                      add a b))
eval (Lam x v) e    = unitId (Fun (\a -> eval v ((x,a):e)))
eval (App t u) e    = eval t e `bindId` (\f ->
                      eval u e `bindId` (\a ->
                      apply f a))
                      
lookup'              :: Name -> Environment -> Identity Value
lookup' x []         = unitId Wrong
lookup' x ((y,b):e)  = if x == y then unitId b else lookup' x e

add                :: Value -> Value -> Identity Value
add (Num i) (Num j) = unitId (Num (i+j))
add a b             = unitId Wrong

apply               :: Value -> Value -> Identity Value
apply (Fun k) a     = k a
apply f a           = unitId Wrong

test                :: Term -> String
test t              = showId (eval t [])

term0 = (   App (Lam "x" (Add (Var "x") (Var "x"))) -- the lambda fn
            (Add (Con 10) (Con 11))   )             -- the parameters