module Primitives2Consumers where
    
import Control.Monad (forever)
import Pipes (Consumer, await, lift)

findFirst :: Monad m => (a -> Bool) -> Consumer a m a
findFirst p = do
    m <- await
    if p m
        then return m
        else findFirst p
        
printThings :: Show a -> Consumer a IO ()
printThings = forever $ do
    v <- await
    lift (print v)