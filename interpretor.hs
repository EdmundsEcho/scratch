-- 

import Control.Monad.List

type Lio = ListT IO

test :: Lio ()
test = do
    x <- return [1,2]
    y <- ListT $ return ['a','b']
    lift $ putStrLn (show (x,y))
    
main = runListT test