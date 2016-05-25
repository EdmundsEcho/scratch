import Pipes 
import qualified Pipes.Prelude as P 

diff :: (Num a, Monad m) => Pipe a a m r 
diff = await >>= loop 
  where 
    loop p = do 
        n <- await 
        yield (n - p) 
        loop n 

main :: IO () 
main = runEffect $ each input >-> diff >-> P.print 
  where 
    input :: [Int] 
    input = [1, 1, 2, 3, 5, 8, 13, 21] 