-- work with Pipes, not just Producers and Consumers

import Control.Monad (replicateM_)
import Pipes
import Prelude hiding (take)
import Control.Applicative ((<$))  -- (<$) modifies return values
import qualified Pipes.Prelude as P
import System.IO

-- main = runEffect $ P.stdinLn >-> P.stdoutLn

main = do
     hSetBuffering stdout NoBuffering
     str <- runEffect $
         ("End of input!" <$ P.stdinLn) >-> ("Broken pipe!" <$ P.stdoutLn)
     hPutStrLn stderr str
     
{-
 $ ./echo3
 Test<Enter>
 Test
 <Ctrl-D>
 End of input!
 $ ./echo3 | perl -e 'close STDIN'
 Test<Enter>
 Broken pipe!
 
 -}
 
--              +--------- A 'Pipe' that
--              |    +---- 'await's 'a's and
--              |    | +-- 'yield's 'a's
--              |    | |
--              v    v v
take ::  Int -> Pipe a a IO ()
take n = do
    replicateM_ n $ do                     -- Repeat this block 'n' times
        x <- await                         -- 'await' a value of type 'a'
        yield x                            -- 'yield' a value of type 'a'
    lift $ putStrLn "You shall not pass!"  -- Fly, you fools!
    
maxInput :: Int -> Producer String IO ()
maxInput n = P.stdinLn >-> take n

maxOutput :: Int -> Consumer String IO ()
maxOutput n = take n >-> P.stdoutLn

-- Each give the same result
-- runEffect $ maxInput 3 >-> P.stdoutLn
-- runEffect $ P.stdinLn >-> maxOutput 3