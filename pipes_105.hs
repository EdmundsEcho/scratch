import Pipes
import Pipes.Prelude
import System.IO (isEOF)

--yield
yieldZero :: Monad m => Producer String m ()
yieldZero = return ()

yieldOne :: Monad m => Producer String m ()
yieldOne = yield "Hello"

yieldTwo :: Monad m => Producer String m ()
yieldTwo = do
  yield "Hello"
  yield "World"
-- yeldTwo = yield "Hello" >> yield "World"

yieldFour :: Monad m => Producer String m ()
yieldFour = do
  yieldTwo
  yieldTwo

-- made yieldTwo polymorphic
twice :: Monad m => a -> Producer a m ()
twice a = do
  yield a
  yield a

echoTwice :: Effect IO ()
echoTwice = for (for stdinLn twice) useString

echoTwice' :: Effect IO ()
echoTwice' = for stdinLn $ \str1 -> for (twice str1) useString

useString str = lift (putStrLn str)

-- stdinLn :: Producer String IO ()
-- stdinLn = do
--   eof <- lift isEOF
--   if eof
--     then return ()
--     else do
--       str <- lift getLine
--       yield str
--       stdinLn

echo :: Effect IO ()
echo = for stdinLn useString

main :: IO ()
main = runEffect echo
