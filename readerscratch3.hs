{-# LANGUAGE InstanceSigs #-}
-- Reader
-- a is the result
-- r is what we read in
newtype Reader r a =
    Reader { runReader :: r -> a }
    
instance Functor (Reader r) where 
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader $ (f . ra)
    -- Reader $ \r -> f (ra r)
    -- because fmap and (.) are one in the same
    
    -- 1. extract r -> a inside Reader
    -- 2. compose it with f
    -- 3. put the composed function back inside Reader
    -- Without the Reader newtype, drop 1-3 and we have
    -- composition.
    
ask :: Reader a a
ask = _

-- a a
-- r -> a
-- a -> a

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName 
                     , dogName :: DogName
                     , address :: Address
                     } deriving (Eq, Show)
                     
data Dog = Dog { dogsName :: DogName
               , dogsAddress :: Address 
               } deriving (Eq, Show)
               
pers :: Person
pers = Person (HumanName "Big Bird") 
              (DogName "Barkley")
              (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") 
               (DogName "Papu") 
               (Address "Austin")
               
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- Reader is just Applicative Functor 
-- on two partially applied functions
-- that need a pointer to the Person
-- it represents...
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address
