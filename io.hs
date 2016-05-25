putStr' :: String -> IO ()    
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' "\n"

getLine' :: IO String
getLine' = get []

get :: String -> IO String
get xs = do x <- getChar
            case x of
                '\r' -> return xs
                _ -> get (xs ++ [x])
                
interact' :: (String -> String) -> IO ()
interact' f = do input <- getLine'
                 putStrLn' (f input)

main = interact' (\x -> x ++ " argh!")
inp = "this  \n\
                 \  that"