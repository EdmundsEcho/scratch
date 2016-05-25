{-# LANGUAGE ScopedTypeVariables #-}

import Pipes
import Pipes.Prelude (stdinLn, stdoutLn)
import Prelude hiding (map)
import Control.Monad (forever)

map :: forall m a b. Monad m => (a -> b) -> Pipe a b m ()
map f = forever $ do
  a <- await
  (yield . f) a
  --yield (f a)

  where
    _ = f :: a -> b
    _ = yield :: b -> Pipe a b m ()
  --  _ = a :: Pipe     a b m a

main :: IO ()
main = runEffect (stdinLn >-> map (++ "!") >-> stdoutLn)

-- lift :: IO r -> Producer a IO r
-- lift :: IO r -> Consumer a IO r
-- lift :: IO r -> Effect     IO r
-- await :: Consumer a   m a
-- await :: Pipe     a b m a
-- yield :: b -> Producer b m ()
-- yield :: b -> Pipe   a b m ()
