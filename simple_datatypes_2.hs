-- if the counts could overflow,
-- then the farm can afford the
-- programmer time to convert
-- the system
newtype NumCow = NumCow Int deriving (Eq, Show)

newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

data GuessWhat = Chickenbutt deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

data RecordProduct a b = 
        RecordProduct { pfirst :: a
                      , psecond :: b } deriving (Eq, Show)
                      
newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

--type Name = String
type Age = Int
type LovesMud = Bool
-- Sheep can produce between 2 and 30
-- pounds (0.9 and 13 kilos) of wool per year!
-- Icelandic sheep don't produce as much
-- wool per year as other breeds but the
-- wool they do produce is a finer wool.
type PoundsOfWool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)
data Animal = Cow CowInfo
            | Pig PigInfo
            | Sheep SheepInfo deriving (Eq, Show)
            
trivialValue :: GuessWhat
trivialValue = Chickenbutt

-- must be applied to something to generate a concrete type
data Id a = MkId a deriving (Eq, Show)
idInt :: Id Integer 
idInt = MkId 10

-- study this
-- Id takes an argument
-- matches with MkId that also takes an argument
-- as I have programmed with \x
-- This is an important pattern for deriving
-- code.
idIdentity :: Id (a -> a)
idIdentity = MkId (\x -> x)
-- when I pass an arg, I apply away a
-- and end up with a

-- term: is a "field" in the data type (sum or product)
-- type synonyms; now I can use either the synonym or
-- datatypes to define the *term*
type Awesome = Bool
type Name = String 

-- using the *product* constructor
person :: Product Name Awesome
person = Product "Eliot" True

data Twitter = Twitter deriving (Eq, Show) 
data AskFm = AskFm deriving (Eq, Show) 

-- Using type synonym AND
-- using "string" as a constructor
-- will not give the type system what we need
-- to differentiate.
--type Twitter = String 
--type AskFm = String

-- using the *sum* constructor
-- this is a biforcation; choice
-- First is mapped to Twitter
-- Second mapped to AskFm
-- using the following
socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter
-- Sum tells Haskell which arg goes with
-- the data constructor First and which
-- with the constructor Second.
-- This is a sum type
--data SocialNetwork = Twitter
--                   | AskFm deriving (Eq, Show)
twitter :: Sum Twitter AskFm
twitter = First Twitter

askfm :: Sum Twitter AskFm
askfm = Second AskFm

type MyInt = Int
getMyInt :: Int -> MyInt
getMyInt n = n

getMyString :: String
getMyString = "String"

getMyChars :: [Char]
getMyChars = "[Char]"

