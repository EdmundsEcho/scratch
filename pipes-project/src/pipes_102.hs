import Pipes
--import qualified Pipes.Text as Text
--import qualified Pipes.Text.IO as Text
--import Pipes.Safe

import Control.Applicative
import Control.Monad (unless)
import System.IO (isEOF)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

main =  do putStrLn "Hello"
    
    --runSafeT $ runEffect $ Text.readFile "data.txt" 
    --    >-> Text.writeFile "outFile.txt"