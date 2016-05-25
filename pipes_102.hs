import Pipes
--import Pipes.Safe
import qualified Pipes.Text as Text
import qualified Pipes.Text.IO as Text
import qualified Pipes.Prelude as P

import Control.Monad (unless)
import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G
import System.IO (isEOF)

   
 --runSafeT $ runEffect $ 
 --        Text.readFile  "/Users/edmund/Downloads/Exeltis_Copay_01122016.csv" 
 --    >-> Text.writeFile "/Users/edmund/Downloads/outFile.csv"
  
-- this is the producer; the type includes my data in this case String        
stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine 
        yield str
        stdinLn
        
-- exits if and when await fails to return anything from the producer
-- await is like getLine - it does not take any arguments.  It makes 
-- you wonder what it is bound to.
stdoutLn :: Consumer String IO ()
stdoutLn = do
    str <- await
    x   <- lift $ try $ putStrLn str
    case x of
        -- gracefully terminate if we have a broken pipe error
        Left e@(G.IOError { G.ioe_type = t}) ->
            lift $ unless (t == G.ResourceVanished) $ throwIO e
        -- Otherwise loop
        Right () -> stdoutLn

-- this is my monadic function, the effect on my data a -> m b
-- the retun value is an Effect.  The end of the chain.
-- link to producer stdinLn
--loop :: Effect IO ()
--loop = for stdinLn $ \str -> do
--    lift $ putStrLn str
-- concise: loop = for stdinLn (lift . putStrLn)
    
--runEffect :: Monad m => Effect m r -> m r

triple :: Monad m => a -> Producer a m ()
triple x = do
    yield x
    yield x
    
doubleUp :: Monad m => Consumer String m String
doubleUp = do
    str1 <- await
    str2 <- await
    return (str1 ++ str2)
-- concise: doubleUp = (++) <$> await <*> await
     
loop :: Producer String IO ()
loop = for P.stdinLn triple

main :: IO ()
-- nested loop (for for)
--main = runEffect $ for stdinLn $ lift . putStrLn
--main = runEffect $ for (for stdinLn triple) (lift . putStrLn)
main = runEffect $ for stdinLn (triple ~> lift . putStrLn)
-- compare to Consumer driven
-- main = runEffect $ lift getLine >~ doubleUp >~ stdoutLn
