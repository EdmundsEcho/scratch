--main :: IO ()
--main = do
--    input <- getLine 
--    putStrLn ("in: " ++ input)
    
myDo :: Maybe String
myDo = do
    x <- Just 3
    y <- Just "&"
    return (show x ++ y)
    
