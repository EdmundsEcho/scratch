import Pipes
import qualified Pipes.Prelude as P
--import Data.List

duplicate :: (Monad m) => a -> Producer a m ()
duplicate x = do
  yield x
  yield x

loop :: Producer String IO ()
loop = for P.stdinLn duplicate

main :: IO ()
main = runEffect $ for loop (lift.putStrLn)
-- main = runEffect $ for P.stdinLn (duplicate ~> lift . putStrLn)
--
-- main = runEffect $
--   for P.stdinLn $ \str1 ->
--     for (duplicate str1) $ \str2 ->
--       lift $ putStrLn str2

--
average :: (Fractional a, Foldable t) => t a -> a
average xs = (sum xs) / (fromIntegral (length xs))
