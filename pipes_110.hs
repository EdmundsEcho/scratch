{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Pipes.Csv (decodeByName)
import Pipes.ByteString (ByteString, fromHandle)
import Data.Csv ((.:), FromNamedRecord(..))
import Pipes
import Control.Applicative
import System.IO hiding (stdin)
import GHC.Generics

data File = File {name::String} deriving (Show)
inFile :: File
inFile  = File "data_v1.csv"

data Rec = R   { uid  :: !Uid
               , specialty :: !Specialty
               , m1 :: !Double
               , m2 :: !Double
               , m3 :: !Double
               , m4 :: !Double
               , m5 :: !Double
               } deriving (Show, Generic)

type Uid = String; type Specialty = String

instance FromNamedRecord Rec

records :: Monad m
        => Producer ByteString m ()
        -> Producer (Either String Rec) m ()
records = decodeByName

main :: IO ()
main =
  withFile (name inFile) ReadMode  $ \hIn  ->
  runEffect $ for (records (fromHandle hIn)) (lift . print)
