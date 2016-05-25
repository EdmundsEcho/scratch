-- monad

type Name           = String

data Term           = Var Name
                    | Con Int
                    | Add Term Term
                    | Lam Name Term -- var name
                    | App Term Term
                    deriving (Show)
                    
data Value          = Wrong
                    | Num Int
                    | Fun (Value -> E Value)
                    
type Environment = [(Name, Value)]

-- monad: Error messages feature
-- if m :: E a and k :: a -> a -> E b
-- then m `bindE` k acts a strict postfix application
-- if m succeeds then k is applied to the successful result
-- if m fails, then so does the application
data E a = Success a | Error String

unitE :: a -> E a
unitE a  = Success a
errorE s = Error s

bindE :: E a -> (a -> E b) -> E b
(Success a) `bindE` k = k a
(Error s)   `bindE` k = Error s

showE :: E Value -> String
showE (Success a) = "Success: " ++ showval a
showE (Error s)   = "Error: " ++ s
-- End monad

showval             :: Value -> String
showval Wrong       = "<wrong>"
showval (Num i)     = show i
showval (Fun f)     = "<function>"

-- my interpreter
eval                :: Term -> Environment -> E Value
eval (Var x) e      = lookup' x e
eval (Con i) e      = unitE (Num i)
eval (Add u v) e    = eval u e `bindE` (\a ->
                      eval v e `bindE` (\b ->
                      add a b))
eval (Lam x v) e    = unitE (Fun (\a -> eval v ((x,a):e)))
eval (App t u) e    = eval t e `bindE` (\f ->
                      eval u e `bindE` (\a ->
                      apply f a))
                      
lookup'             :: Name -> Environment -> E Value
lookup' x []        =  errorE ("unbound variable: " ++ x)
lookup' x ((y,b):e) = if x == y then unitE b else lookup' x e

add                 :: Value -> Value -> E Value
add (Num i) (Num j) = unitE (Num (i+j))
add a b             = errorE ("should be numbers: " ++ showval a
                                             ++ "," ++ showval b)

apply               :: Value -> Value -> E Value
apply (Fun k) a     = k a
apply f a           = errorE ("should be function: " ++ showval f)

test                :: Term -> String
test t              = showE (eval t [])

term0 = (   App (Lam "x" (Add (Var "x") (Var "x"))) -- the lambda fn
            (Add (Con 10) (Con 11))   )             -- the parameters