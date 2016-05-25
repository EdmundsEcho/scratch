import Control.Monad.Trans.State.Strict
import Lens.Family.State.Strict
import Pipes
import Pipes.Parse
import Prelude hiding (splitAt)
import qualified Control.Fold1 as L

sum10 :: (Monad m) => Parser Double m Double
sum10 = zoom (splitAt 10) (foldAll (+) 0 id)

main = print $ evalState sum10 (each [1..])
-- prints 55

average :: L.Fold Int Int
average = div <$> L.sum <*> L.length