{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import Control.Applicative
import Control.Exception (throwIO)
import Control.Foldl (Fold)
import Data.ByteString (ByteString)
import Data.Csv (FromNamedRecord(..), ToNamedRecord(..), (.:), (.=))
import Lens.Simple (makeLensesBy)
import Pipes (Effect, Producer, for, lift, yield, (>->))

import qualified Control.Foldl    as Foldl
import qualified Data.Csv         as Csv
import qualified Data.Vector      as Vector
import qualified Pipes
import qualified Pipes.Prelude
import qualified Pipes.ByteString
import qualified Pipes.Csv
import qualified System.IO        as IO

data InputRow = InputRow
    { input_record_id :: Int
    , input_val_t1    :: Double
    , input_val_t2    :: Double
    , input_val_t3    :: Double
    , input_val_t4    :: Double
    , input_val_t5    :: Double
    }

instance FromNamedRecord InputRow where
parseNamedRecord m = do
    InputRow
        <$> (m .: "record_id")
        <*> (m .: "val_t1"   )
        <*> (m .: "val_t2"   )
        <*> (m .: "val_t3"   )
        <*> (m .: "val_t4"   )
        <*> (m .: "val_t5"   )

makeLensesBy (\n -> Just (n ++ "_lens")) ''InputRow

data OutputRow = OutputRow
    { output_record_id :: Int
    , output_val_t1    :: Double
    , output_val_t2    :: Double
    , output_val_t3    :: Double
    , output_val_t4    :: Double
    , output_val_t5    :: Double
    , output_avg_t1_t5 :: Double
    }

instance ToNamedRecord OutputRow where
toNamedRecord outputRow =
    Csv.namedRecord
        [ "record_id" .= output_record_id outputRow
        , "val_t1"    .= output_val_t1    outputRow
        , "val_t2"    .= output_val_t2    outputRow
        , "val_t3"    .= output_val_t3    outputRow
        , "val_t4"    .= output_val_t4    outputRow
        , "val_t5"    .= output_val_t5    outputRow
        , "avg_t1_t5" .= output_avg_t1_t5 outputRow
        ]

fold :: Fold InputRow (Maybe OutputRow)
fold =
    f   <$> Foldl.last
        <*> Foldl.handles input_val_t1_lens Foldl.sum
        <*> Foldl.handles input_val_t2_lens Foldl.sum
        <*> Foldl.handles input_val_t3_lens Foldl.sum
        <*> Foldl.handles input_val_t4_lens Foldl.sum
        <*> Foldl.handles input_val_t5_lens Foldl.sum
        <*> Foldl.genericLength
    where
    f mInputRow sum_t1 sum_t2 sum_t3 sum_t4 sum_t5 len = do
        inputRow <- mInputRow
        let avg = (sum_t1 + sum_t2 + sum_t3 + sum_t4 + sum_t5) / (5 * len)
        let outputRow =
                OutputRow
                    { output_record_id = input_record_id inputRow
                    , output_val_t1    = input_val_t1    inputRow
                    , output_val_t2    = input_val_t2    inputRow
                    , output_val_t3    = input_val_t3    inputRow
                    , output_val_t4    = input_val_t4    inputRow
                    , output_val_t5    = input_val_t5    inputRow
                    , output_avg_t1_t5 = avg
                    }
        return outputRow

main =
  IO.withFile "data_v2.csv"  IO.ReadMode  (\handleIn  -> do
  IO.withFile "output.csv" IO.WriteMode (\handleOut -> do

  let input :: Producer ByteString IO ()
      input = Pipes.ByteString.fromHandle handleIn

  let handleRow :: Either String InputRow -> Producer InputRow IO ()
      handleRow (Left str)  = lift (throwIO (userError str))
      handleRow (Right row) = yield row

  let inputRows :: Producer InputRow IO ()
      inputRows = for (Pipes.Csv.decodeByName input) handleRow

  let outputRows :: Producer OutputRow IO ()
      outputRows =
              inputRows
          >-> Foldl.purely Pipes.Prelude.scan fold
          >-> Pipes.Prelude.concat

  let header =
          Vector.fromList
              [ "record_id"
              , "val_t1"
              , "val_t2"
              , "val_t3"
              , "val_t4"
              , "val_t5"
              , "avg_t1_t5"
              ]

  let output :: Producer ByteString IO ()
      output = outputRows >-> Pipes.Csv.encodeByName header

  let effect :: Effect IO ()
      effect = output >-> Pipes.ByteString.toHandle handleOut

  Pipes.runEffect effect ))
