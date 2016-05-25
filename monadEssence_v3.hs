-- monad

type Name           = String
type Position       = String

data Term           = Var Name
                    | Con Int
                    | Add Term Term
                    | Lam Name Term -- var name
                    | App Term Term
                    | At Position Term
                    deriving (Show)
                    
data Value          = Wrong
                    | Num Int
                    | Fun (Value -> E Value)
                    
type Environment = [(Name, Value)]

type Identity a = a

-- monad: Error messages feature
-- if m :: E a and k :: a -> a -> E b
-- then m `bindE` k acts a strict postfix application
-- if m succeeds then k is applied to the successful result
-- if m fails, then so does the application
data E a = Success a | Error String
unitE a  = Success a
errorE s = Error s

-- monad: Error and position
type P a = Position -> E a

unitP a  = \p -> unitE a

errorP s = \p -> errorE (showP p ++ ": " ++ s)

m `bindP` k = \p -> m p `bindE` (\x -> k x p)

showP m = showE (m pos0)

resetP :: Position -> P x -> P x
resetP q m = \p -> m q

(Success a) `bindE` k = k a
(Error s) `bindE` k = Error s

showE (Success a) = "Success: " ++ showval a
showE (Error s)   = "Error: " ++ s
-- End monad

showval             :: Value -> String
showval Wrong       = "<wrong>"
showval (Num i)     = show i
showval (Fun f)     = "<function>"

-- my interpreter
eval                :: Term -> Environment -> P Value
eval (Var x) e      = lookup' x e
eval (Con i) e      = unitP (Num i)
eval (Add u v) e    = eval u e `bindP` (\a ->
                      eval v e `bindP` (\b ->
                      add a b))
eval (Lam x v) e    = unitP (Fun (\a -> eval v ((x,a):e)))
eval (App t u) e    = eval t e `bindP` (\f ->
                      eval u e `bindP` (\a ->
                      apply f a))
eval (At p t) e     = resetP p (eval t e)
                      
lookup'             :: Name -> Environment -> P Value
lookup' x []        =  errorP ("unbound variable: " ++ x)
lookup' x ((y,b):e) = if x == y then unitP b else lookup' x e

add                 :: Value -> Value -> P Value
add (Num i) (Num j) = unitP (Num (i+j))
add a b             = errorP ("should be numbers: " ++ showval a
                                             ++ "," ++ showval b)

apply               :: Value -> Value -> P Value
apply (Fun k) a     = k a
apply f a           = errorP ("should be function: " ++ showval f)

test                :: Term -> String
test t              = showE (eval t [])

term0 = (   App (Lam "x" (Add (Var "x") (Var "x"))) -- the lambda fn
            (Add (Con 10) (Con 11))   )             -- the parameters
            
pos0 = undefined