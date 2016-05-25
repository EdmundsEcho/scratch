{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Pipes.Csv (decodeByName)
import Pipes.ByteString (ByteString, fromHandle)
import Data.Csv ((.:), FromNamedRecord(..))
import Pipes
import Control.Applicative
import System.IO hiding (stdin)
import GHC.Generics
import Control.Foldl (purely)
import Pipes.Prelude (fold,stdoutLn)
import Control.Foldl


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

data RecP = RP { uidP  :: !Uid
               , specialtyP :: !Specialty
               , timeSeriesP :: TimeSeries
               } deriving (Show)

type Uid = String; type Specialty = String
type TimeSeries = [Double]

instance FromNamedRecord Rec

records :: Monad m
        => Producer ByteString m ()
        -> Producer (Either String Rec) m ()
records = decodeByName

main :: IO ()
main =
  withFile (name inFile) ReadMode  $ \hIn  ->
  runEffect $
  (records (fromHandle hIn)
      >-> duplicate
      >-> stdoutLn)

toP :: Rec -> RecP
toP r = RP    (uid r)
              (specialty r)
              [m1 r,m2 r,m3 r,m4 r,m5 r]

duplicate :: (Monad m) => a -> Producer a m ()
duplicate x = do
  yield x
  yield x

-- toP :: (Monad m) => a -> Producer a m ()
-- toP r = do
--    yield _
--    RP (uid r)
--       (specialty r)
--       [m1 r,m2 r,m3 r,m4 r,m5 r]
--

-- minMax :: Ord a => Fold a (Maybe a, Maybe a)
-- minMax = liftA2 (,) minimum maximum
