{-# LANGUAGE OverloadedStrings #-}

import Pipes.Csv (decodeByName)
import Pipes.ByteString (ByteString, fromHandle)
import Data.Csv ((.:), FromNamedRecord(..))
import Pipes
import Control.Applicative
import System.IO hiding (stdin)

data File = File {name::String} deriving (Show)
inFile :: File
inFile  = File "/Users/edmundcape/Downloads/names.txt"

data Person = Person { fname :: String
                     , age   :: Int } deriving (Show)

instance FromNamedRecord Person where
  parseNamedRecord p =
    Person <$> p .: "name"
           <*> p .: "age"

persons :: Monad m
        => Producer ByteString m ()
        -> Producer (Either String Person) m ()
persons = decodeByName

main :: IO ()
main =
  withFile (name inFile) ReadMode  $ \hIn  ->
  runEffect $ for (persons (fromHandle hIn)) (lift . print)
