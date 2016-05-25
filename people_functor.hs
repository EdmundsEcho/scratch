-- People Maybe program
--data Person = PersonWithAge String Int 
--            | PersonWithoutAge String
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Monoid
           
data Person = Person { getName :: String
                     , getAge  :: Maybe Int
                     , getIncome :: Maybe Int } deriving (Show)
                     
pat :: Person
pat = Person "Pat" (Just 23) (Just 100)

jim :: Person
jim = Person "Jim" (Just 34) (Just 150)

persons :: [Person]
persons = [pat, jim]

liftedFpat = fmap (+) (getAge pat)
liftedFjim = fmap (+) (getAge jim)

data User = User 
    String  -- Name
    String  -- email
    Int     -- Age
    UTCTime -- DOB

parseJSON :: Value -> Parser User
parseJSON (Object o) = User
    <$> o .: "name"
    <*> o .: "email"
    <*> o .: "age"
    <*> o .: "birth_date"
parseJSON _ = mzero
    
-- the constructor
-- User :: String -> String -> User


--fmap (fmap (7+)) ages

--sumAge :: [Person] -> Int
--sumAge xs = foldl (+) 0 xs