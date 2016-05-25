-- Scratch pad for ALlen

import Control.Applicative
import Control.Monad (join) 

-- nullary
data GuessWhat = Chickenbutt deriving (Eq, Show) 
-- Chickenbutt :: Chickenbutt

-- constructor
trivialValue :: GuessWhat   -- type
trivialValue = Chickenbutt  -- constructor

-- unary
data Id a = MkId a deriving (Eq, Show) 
-- MkId :: a -> Id a

-- constructor
idInt :: Id Int
idInt = MkId 10

idIdentity :: Id (a -> a)
idIdentity = MkId (\x -> x)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a
             | Second b deriving (Eq, Show)
             
data RecordProduct a b = RecordProduct 
    {  pfirst :: a
     , psecond::b } deriving (Eq, Show)
     
-- RecordProduct :: a -> b -> RecordProduct a b

-- Animal counts
newtype NumCow = 
    NumCow Int deriving (Eq, Show)
    
newtype NumPig = 
    NumPig Int deriving (Eq, Show)

newtype NumSheep = 
    NumSheep Int deriving (Eq, Show)    
    
-- Farmhouse
data Farmhouse = 
    Farmhouse NumCow NumPig deriving (Eq, Show)
    
type Farmhouse1 = 
    Product NumCow NumPig
    
data BigFarmhouse = 
    BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
    
type BigFarmhouse' = 
    Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool
-- Sheep can produce between 2 and 30
-- pounds (0.9 and 13 kilos) of wool per year!
-- Icelandic sheep don't produce as much
-- wool per year as other breeds but the
-- wool they do produce is a finer wool.
type PoundsOfWool = Int

data CowInfo = 
    CowInfo Name Age deriving (Eq, Show)

data PigInfo =
    PigInfo Name Age LovesMud deriving (Eq, Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo
            | Pig PigInfo
            | Sheep SheepInfo deriving (Eq, Show)
            
type Awesome = Bool
--type Name = String

person :: Product Name Awesome 
person = Product "Simon" True

data Twitter =
    Twitter deriving (Eq, Show)
data AskFm =
    AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill | Mac
                     | Windows deriving (Eq, Show)
                     
data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript deriving (Eq, Show)
                         
data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgrammingLanguage } 
                             deriving (Eq, Show)
-- Programmer :: OperatingSystem -> ProgrammingLanguage -> Programmer
            
allOperatingSystems :: [OperatingSystem] 
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill 
    , Mac
    , Windows ]
    
allLanguages :: [ProgrammingLanguage] 
allLanguages =  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer] 
allProgrammers = [Programmer os lang | os <- allOperatingSystems
                                     , lang <- allLanguages]
                                     
data Car = Car { make  :: String 
               , model :: String
               , year  :: Integer } deriving (Eq, Show)
               
-- The Null is still not great, but 
-- we're leaving it in to make a point 
data Automobile = Null
                | Automobile Car deriving (Eq, Show)
                
-- DONT do this
{-
data Automobile = Null
                | Car { make :: String
                      , model :: String
                      , year :: Integer } deriving (Eq, Show)-}
                      
data Quantum = Yes
             | No
             | Both deriving (Eq, Show)
             
-- higher kinded type
-- :kind * -> *
-- leave a whole for user to fill
-- type argument "a" is the hole
--data EsResultFound a = EsResultFound { _version :: DocVersion
--                                     , _source :: a } deriving (Eq, Show)

-- anticipate how the hole might be filled
-- FromJSON deserializes JSON to Haskell
-- FromJSON typeclass instance for EsResultFound
-- requires a FromJSON instance for that a
-- Convention: we don't constrain datatypes; leave a poly
--instance (FromJSON a) => FromJSON (EsResultFound a) where 
--    parseJSON (Object v) = EsResultFound <$> 
--                           v .: "_version" <*> 
--                           v .: "_source" 
--    parseJSON _ = empty

data BinaryTree a = Leaf -- terminal end with no value
                  | Node (BinaryTree a) a (BinaryTree a) 
                  deriving (Eq, Ord, Show)
                  
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) = Node
                                 (mapTree f left)
                                 (f x)
                                 (mapTree f right)
 
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a 
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) | b == a = Node left a right
                              | b < a  = Node(insert' b left) a right 
                              | b > a  = Node left a (insert' b right)  
                              
mtree = (Node Leaf 5 Leaf)
mtree' = insert' 8 mtree               

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = [x]
                             ++ preorder left
                             ++ preorder right

inorder :: BinaryTree a -> [a]
inorder = undefined

postorder :: Ord a => BinaryTree a -> [a]
postorder = undefined 

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO () 
testPreorder =
    if preorder testTree == [2, 1, 3] 
        then putStrLn "Preorder fine!" 
        else putStrLn "Bad news bears."

testInorder :: IO () 
testInorder =
    if inorder testTree == [1, 2, 3] 
        then putStrLn "Inorder fine!" 
        else putStrLn "Bad news bears."
        
testPostorder :: IO () 
testPostorder =
    if postorder testTree == [1, 3, 2] 
        then putStrLn "Postorder fine!"
        else putStrLn "postorder failed check"        
        
data CountingBad a = Heisenberg Int a deriving (Eq, Show)
instance Functor CountingBad where
    fmap f (Heisenberg n x) = Heisenberg (n+1) (f x)
    
-- test :: f a -> CountingBad a
-- test n x = Heisenberg n x

-- Functors
replaceWithP :: b -> Char
replaceWithP = const 'p' 

-- assert more
replaceWithP' :: [Maybe[Char]] -> Char
replaceWithP' = replaceWithP

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

liftedReplace :: Functor f => 
    f a -> f Char 
liftedReplace = fmap replaceWithP

-- assert more
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f, Functor f1) => 
    f(f1 a) -> f(f1 Char)
twiceLifted = (fmap.fmap) replaceWithP

-- assert more
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

-- maintains all layers
thriceLifted :: (Functor f, Functor f1, Functor f2) => 
    f(f1(f2 a)) -> f(f1(f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP
    
-- assert more
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted
    
main :: IO () 
main = do
    putStr "replaceWithP' lms:   " 
    print (replaceWithP' lms)
    putStr "liftedReplacelms:    "
    print (liftedReplace lms)
    
    putStr "liftedReplace' lms:  "
    print (liftedReplace' lms)
    putStr "twiceLifted lms:     "
    print (twiceLifted lms)
    putStr "twiceLifted' lms:    "
    print (twiceLifted' lms)
    putStr "thriceLifted lms:    "
    print (thriceLifted lms)
    putStr "thriceLifted' lms:   " 
    print (thriceLifted' lms)
    
-- Phantom types; in this case b
-- this definition echos the functio const
newtype Constant a b = Constant { getConstant :: a } 
                       deriving (Eq, Show)

-- k:: * -> * -> *
-- not a Functor so need to parially apply
instance Functor (Constant m) where
    fmap _ (Constant v) = Constant v 
    
--liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")] 
h z = lookup z [(2,3),(5,6),(7,8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where 
        fmap f (Identity a) = Identity (f a)
        
instance Applicative Identity where 
        pure a = Identity a
        Identity f <*> Identity a = Identity (f a)
        

--join :: Monad m   => m (m a) -> m a
--fmap :: Functor f => (a -> b) -> f a -> f b

-- implement in terms of fmap and join
bind :: Monad m => (a -> m b) -> m a -> m b 
bind f s = join (fmap f s)

twiceWhenEven :: [Integer] -> [Integer] 
twiceWhenEven xs = do
    x <- xs 
    if even x
        then [x*x, x*x] 
        else []
