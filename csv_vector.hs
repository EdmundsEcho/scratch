import Data.Csv
import Pipes (Effect, Producer, for, lift, yield, (>->))
import Data.ByteString

import qualified Data.Vector as V
import qualified System.IO   as IO
import qualified Pipes
import qualified Pipes.Prelude
import qualified Pipes.ByteString
import qualified Pipes.Csv

data File = File {name::String} deriving (Show)
-- generates accessors name:: File -> String

inFile  = File "ata_v2.csv"
outFile = File "outFile.csv"

data InputRow = InputRow
               { uid  :: !Uid
               , specialty :: !Specialty
               , timeSeries :: TimeSeries
               } deriving (Show)

data TimeSeries = TimeSeries [Double] deriving (Show)
type Uid = String
type Specialty = String


csvBSToRecs :: ByteString -> Either String (V.Vector InputRow)
csvBSToRecs b =
     case (decode NoHeader b :: Either String (V.Vector (V.Vector ByteString))) of
       Left s  -> Left s
       Right v -> Right (V.map
                        (\r -> InputRow
                               (r V.! 0)
                               (r V.! 1)
                               TimeSeries (V.toList $ V.drop 2 r)) v)

main =
   IO.withFile "data_v2.csv" IO.ReadMode (\handleIn  -> do
   IO.withFile "output.csv" IO.WriteMode (\handleOut -> do

   let input :: Producer ByteString IO ()
       input = Pipes.ByteString.fromHandle handleIn

   let outputRows :: Producer OutputRow IO ()
   outputRows =
     inputRows
     >-> Foldl.purely Pipes.Prelude.scan fold
     >-> Pipes.Prelude.concat

   let output :: Producer ByteString IO ()
       output = csvBSToRecs >-> encode NoHeader

   let effect :: Effect IO ()
       effect = output >-> Pipes.ByteString.toHandle handleOut

   Pipes.runEffect effect ))
