-- monad take 2
-- we add a monad
-- and update the evaluator with unit
-- In this version we add
-- unit
-- bind
-- out

data Term = Con Int | Div Term Term deriving (Show)

-- Monad
-- What is the relationship between
-- the monad and the evaluation of the
-- expression
type M a = (Output,a)
type Output = String

unit :: a -> M a
unit a = ("",a)

bind :: M a -> (a -> M b) -> M b
m `bind` k = let (o ,a) = m
                 (o',b) = k a
             in (o ++ o', b)
-- returns the computation with
-- output o and empty value ()
out :: Output -> M ()
out o = (o,())
-- End Monad

eval :: Term -> M Int
eval (Con a)   = out(line(Con a)a) `bind` \() ->
                 unit a
eval (Div t u) = eval t `bind` \a ->
                 eval u `bind` \b ->
                 out(line(Div t u)(a`div`b)) `bind` \() ->
                 unit (a`div`b)

        -- alternative to reverse output
        -- line (Div t u) (a`div`b) ++ o' ++ o

line :: Term -> Int -> Output
line t a = "eval(" ++ show t ++ ") <= " ++ show a ++ " \n "

answer, error' :: Term
answer = Div(Div(Con 1972) (Con 2)) (Con 23)
error' = Div(Con 1) (Con 0)

--eval answer
--eval error'
