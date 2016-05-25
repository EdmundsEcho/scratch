import Pipes
import qualified Pipes.ByteString as PB
import qualified Pipes.Parse      as PP
import System.IO

data File = File {name::String} deriving (Show)
-- generates accessors name:: File -> String

inFile  = File "/Users/edmundcape/Downloads/data.csv"
outFile = File "/Users/edmundcape/Downloads/outFile.csv"


main =
   withFile (name inFile) ReadMode  $ \hIn  ->
   withFile (name outFile) WriteMode $ \hOut ->
--   runEffect $ PB.fromHandle hIn >-> PB.toHandle hOut
   runEffect $ for (PB.fromHandle hIn) (lift . print)

   --
  --
  --  main = runEffect $ takeLines 3 PB.stdin >-> PB.stdout
  --    where
  --      takeLines n = PB.unlines . PP.takeFree n . PB.lines
