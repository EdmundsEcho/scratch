{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- pragma that enables newtype to expoit instance
-- definition of the underlying type.  This is good
-- when we want to have all typeclass instances
-- behave the same and allow for exceptions.
{-# LANGUAGE FlexibleInstances #-}
-- allows more than one definition of a class instance

-- working with Chris Allen p 704
-- cardinality of this type is 1
-- the constructor is Nullary
data Example = MakeExample deriving Show

-- For cardinality, unary constructors 
-- are the identity function.
-- argument is a type not a value
-- cardinality of the type is the same as 
-- the type it contains
--data Goats = Goats Int deriving (Eq, Show)

-- Newtype only allows unary constructors


-- Avoid mixing up Int values
newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype Cows = Cows Int deriving (Eq, Show)
--newtype Horses = Horses (Int, String) deriving (Eq, Show)

--tooManyGoats :: Int -> Bool
--tooManyGoats n = n > 42
tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

-- with newtype we can define class instances
class TooMany a where
    tooMany :: a -> Bool
    
instance TooMany Int where
    tooMany n = n > 42
    
instance TooMany (Int, String) where
    tooMany (n, _) = n > 42
    
instance TooMany (Int, Int) where
    tooMany (n1, n2) = (n1 + n2) > 42
    
instance TooMany (Num a) => (a,a) where
    tooMany (n1, n2) = (n1 + n2) > 42

-- the same as Int    
--instance TooMany Goats where
--    tooMany (Goats n) = n > 42
    
-- the exception
instance TooMany Cows where
    tooMany (Cows n) = n > 10


