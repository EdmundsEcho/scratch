import PipesTalk
import Sandwiches (meats, cheeses, breads)

makeSandwich :: IO (String, String, String)
makeSandwich = do
    meat <- pick meats
    cheese <- pick cheeses
    pause
    return (meat, chease, bread)
    
main = do
    v <- makeSandwich
    print v
    
    let printIt mv = do
        v <- mv
        print v
    mapM_ printIt (replicate 5 makeSandwich)