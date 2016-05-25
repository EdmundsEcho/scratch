module Main where

import Control.Applicative
import Control.Monad (unless)
import System.IO (isEOF)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Pipes
import qualified Pipes.Prelude as P  -- or use any other qualifier you prefer

--         +--------+-- A 'Producer' that yields 'String's
 --         |        |
 --         |        |      +-- Every monad transformer has a base monad.
 --         |        |      |   This time the base monad is 'IO'.
 --         |        |      |  
 --         |        |      |  +-- Every monadic action has a return value.
 --         |        |      |  |   This action returns '()' when finished
 --         v        v      v  v
stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift isEOF        -- 'lift' an 'IO' action from the base monad
    unless eof $ do
        str <- lift getLine
        yield str            -- 'yield' the 'String'
        stdinLn              -- Loop
        
         
         
main :: IO ()
main = do
  h <- openFile "../data.txt" ReadMode
  line <- hGetLine h
  putStrLn . show . words $ line
  hClose h