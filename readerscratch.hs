-- Reader
import Control.Applicative

hurr = (*2)
durr = (+10)

m :: Integer -> Integer
m = hurr . durr

-- lift over function where context
-- is the partially applied function
m' :: Integer -> Integer
m' = fmap hurr durr

-- in the Applicative context
-- we send the argument to both functions
-- simultaneously (fmap forced a sequence)
-- compare m 3 with m2 3
m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

hurrDurr :: Integer -> Integer
hurrDurr = do
    a <- hurr
    b <- durr
    return (a + b)