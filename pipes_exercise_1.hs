{-# LANGUAGE ScopedTypeVariables #-}

import Pipes
import Pipes.Prelude (stdinLn, stdoutLn)
import Prelude hiding (takeWhile)

takeWhile :: forall m a. Monad m => (a -> Bool) -> Pipe a a m ()
takeWhile keep = do
  a <- await
  if keep a
    then do
      yield a
      takeWhile keep
  else return ()

main :: IO ()
main = runEffect (stdinLn >-> takeWhile (/= "quit") >-> stdoutLn)

-- lift :: IO r -> Producer a IO r
-- lift :: IO r -> Consumer a IO r
-- lift :: IO r -> Effect     IO r
-- await :: Consumer a   m a
-- await :: Pipe     a b m a
-- yield :: b -> Producer b m ()
-- yield :: b -> Pipe   a b m ()
