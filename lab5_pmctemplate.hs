module Lab5 where

import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action = 
       Atom (IO Action)      -- an atomic computation, returning a new action
     | Fork Action Action    -- create a new thread
     | Stop                  -- terminate this thread
     
writeAction :: String -> Action 
writeAction ""     = Stop
writeAction (c:cs) = Atom $ do
   putChar c
   return $ writeAction cs
   
prog :: Action
prog = Fork (writeAction "Hello\n") (writeAction "CIS 552\n")

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================
-- action :: Concurrent a -> Action
-- ((a -> Action) -> Action) -> Action
-- uses Stop :: Action  
-- action :: ((a -> Action) -> Action) -> Action
-- fn :: ((a -> b) -> b) -> b
action :: Concurrent a -> Action
action Concurrent (\a -> Atom) = c f (\a -> Stop)
{-
action (Concurrent (\a -> Fork Stop $ Fork Stop Stop))
action (Concurrent (\a -> Stop))
action (Concurrent (\a -> Atom $ putStr "Haskell"))
action (Concurrent (\a -> Atom $ putStr "Haskell" >> return Stop))
action (fork (atom (putStr "Hacker")))
action $ fork stop
action $ atom undefined
action (fork (atom (putStr "Hacker")))
action $ par stop stop
action (par (atom (putStr "think")) (atom (putStr "hack")))
action (par stop $ fork stop)
action $ par (atom $ putChar 'x') (fork stop)
action (stop >>= (\c -> stop))
action (fork stop >>= \_ -> fork stop)
run ex0
-}
-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = (\c -> Stop)

-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
atom = error "You have to implement atom"


-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork m = \c -> Fork (action m) (c())

par :: Concurrent a -> Concurrent a -> Concurrent a
par m1 m2 = \c -> Fork (m1, c) (m2, c)


-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
    f >>= k = Concurrent (\c -> f (\a -> k a c))
    return x = Concurrent (\c -> c x)
-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
--round :: Monad m => [Action m] -> m ()
roundRobin [] = return ()
roundRobin (a:as) = case a of
    Atom am     -> do a' <- am 
                      roundRobin (as ++ [a'])
    Fork a1 a2  -> roundRobin (as ++ [a1, a2])
    Stop        -> roundRobin as

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

