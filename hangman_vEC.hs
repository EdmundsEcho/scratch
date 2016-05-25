--  hangman
import System.IO

hangman :: IO ()
hangman =
    do putStr "Think of a word: "
       word <- getLine
       putStr "Try and guess the word: "
       guess word
       
       
guess :: String -> IO ()
guess word = do putStr "> "
                xs <- getLine
                if xs == word then
                    putStrLn "You got it!"
                else 
                    do putStrLn (diff word xs)
                       guess word

diff :: String -> String -> String
diff xs ys = [if (elem x ys) then x else '_' | x <- xs]

sgetline :: IO String
sgetline = do x <- getCh
              if x == '\n' then
                  do putChar x
                     return []
              else
                  do putChar '_'
                     xs <- sgetline
                     return (x:xs)
                     
getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c