-- readFile.hs
 import Pipes
 import qualified Pipes.Prelude as P
 import Pipes.Safe
 import qualified System.IO as IO
 import Prelude hiding (readFile)

 readFile :: FilePath -> Producer' String (SafeT IO) ()
 readFile file = bracket
     (do h <- IO.openFile file IO.ReadMode
         putStrLn $ "{" ++ file ++ " open}"
         return h )
     (\h -> do
         IO.hClose h
         putStrLn $ "{" ++ file ++ " closed}" )
     (\h -> hoist lift (P.fromHandle h))

>>> runSafeT $ run $ readFile "readFile.hs" >-> P.take 4 >-> hoist lift P.stdout
{readFile.hs open}
-- readFile.hs
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Safe
{readFile.hs closed}