module Primitives3Producers where
    
import Pipes (Producer, yield, lift, Pipe, await)

rands :: Producer Float IO ()
rands = forever $ do
    r <- lift (randomRIO (0, 1.0))
    yield r
    
serialize :: (Serialize a, Monad m) => Pipe a ByteString m ()
serialize = forever $ do
    v <- await
    yield "|"
    yield $ encode v