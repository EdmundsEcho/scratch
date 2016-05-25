import System.IO
import Control.Monad()
import Control.Applicative()
import Data.Char (chr)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

data Chunk = Chunk   { chunk :: String }
           | LineEnd { chunk :: String
                     , remainder :: String } deriving (Show)

parseChunk :: B8.ByteString -> Chunk
parseChunk chunk =
    if rightS == B8.pack ""
        then Chunk (toS leftS)
        else LineEnd (toS leftS) ((toS . B8.tail) rightS)
    where
        (leftS, rightS) = B8.break (== '\n') chunk
        toS = map (chr . fromEnum) . B.unpack

main :: IO ()
main = do
  fileH <- openFile "data.txt" ReadMode
  loop "" fileH
  hClose fileH
  where
      loop acc h = do
          isEof <- hIsEOF h
          if isEof
              then do putStrLn acc; putStrLn "DONE..."
              else do
                  --line <- hGetLine h'
                  --print $ words line
                  chunk <- B.hGet h 8
                  case (parseChunk chunk) of
                      (Chunk chunk')
                          -> do
                                let accLine = acc ++ chunk'
                                loop accLine h
                      (LineEnd chunk' remainder)
                          -> do
                                let line = acc ++ chunk'
                                -- process line
                                putStrLn line
                                loop remainder h

main_1 :: IO ()
main_1 = do
  h <- openFile "data.txt" ReadMode
  loop h
  hClose h
  where
      loop h' = do
          isEof <- hIsEOF h'
          if isEof
              then putStrLn "DONE..."
              else do
                  --line <- hGetLine h'
                  --print $ words line
                  chunk <- B.hGet h' 8
                  print . words $ show chunk

                  loop h'
