module Primitives1 where
    
import Pipes (Consumer, await, Producer, yield)

-- types are capped or polymorphic so we cant use them...

-- await' :: Monad m => Proxy () a y' y m a
await' Monad m => Consumer a m a
await' = await

-- yield' :: Monad m => a -> Proxy x' x () a m ()
yield' :: Monad m => b -> Producer b m ()
yield' b = yield b

-- No pipe primitive, Pipe just does both.