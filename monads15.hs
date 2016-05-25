-- Monads in 15 min
-- Choice is the monad wrapper
-- choose is the construtor of Choice a
import Prelude hiding ((>>=), return)

type Choice a = [a]

-- constructor for Choice a
choose :: [a] -> Choice a
choose xs = xs

-- temp function
pair456 :: Int -> Choice (Int, Int) -- a function we use with map
pair456 x = [(x,4),(x,5),(x,6)]

-- Monad: how to process makepairs
join :: Choice (Choice a) -> Choice a
join choices = concat choices

-- map and join are 2/3 of a monad
-- use of the monad improves syntatic sugar
-- before: join (map pair456 (choose [1,2,3]))
-- after:  choose [1,2,3] >>= pair456
-- (>>=) :: m a -> (a -> m b) -> m b
(>>=) :: Choice a -> (a -> Choice b) -> Choice b
choices >>= f = join (map f choices) -- f = something like pair456

-- how do we conveniently construct Choice a
-- we only have one type
-- return :: a -> m a
return :: a -> Choice a
return x = choose [x]

makepairs :: Choice (Int, Int)
makepairs = do
    x <- choose [1,2,3]
    y <- choose [4,5,6]
    return (x,y)
    
mzero :: Choice a
mzero = choose []

-- guard is not a choice, it just takes the result
-- of a test and generates the Choice a result accordingly
-- When fails, the software returns to the previously
-- recorded image of the application (backtracks)
guard :: Bool -> Choice () -- where teh Choice is either
                           -- mzero or not
guard True  = return ()    -- Unit, same as x 1
guard False = mzero

solveConstraint :: Choice (Int, Int)
solveConstraint = do
    x <- choose [1,2,3]
    y <- choose [4,5,6]
    _ <- guard (x*y == 8)
    return (x,y)
    

