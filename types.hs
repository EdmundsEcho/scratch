-- eDx types and powerset
powerset :: [a] -> [[a]]
powerset xs = filterM' (\x -> [True,False]) xs
-- [1,2,3] => 7 sets, e.g., [[1,2,3],[1,2] etc]

-- eDx types
const' :: a -> b -> a
const' x _ = x

-- more explicit: returns a function
const'' :: a -> (b -> a)
const'' x = \_ -> x

sequence_' :: Monad m => [m a] -> m ()
sequence_' ms = foldr (>>) (return ()) ms

--sequence_' [] = return ()
--sequence_' (m:ms) = (foldl (>>) m ms) >> return ()

--x sequence_' ms = foldl (>>) (return ()) ms

--sequence_' [] = return ()
--sequence_' (m:ms) = m >> sequence_' ms

--sequence_' [] = return ()
--sequence_' (m:ms) = m >>= \_ -> sequence_' ms

sequence' :: Monad m => [m a] -> m [a]
sequence' ms = foldr func (return []) ms
    where 
          func :: (Monad m) => m a -> m [a] -> m [a]
          func m acc = do x <- m
                          xs <- acc
                          return (x:xs)

--sequence' [] = return []
--sequence' (m:ms) = m >>= \a ->
--                do as <- sequence' ms
--                   return (a:as)

--sequence' [] = return []
--sequence' (m:ms) = do a <- m
--                      as <- sequence' ms
--                      return (a:as)

--sequence' ms = foldr func (return []) ms
--    where
--        func :: (Monad m) => m a -> m [a] -> m [a]
--        func m acc = m : acc

--sequence' [] = return []
--sequence' (m:ms) = m >>= \a ->
--        do as <- sequence' ms
--           return (a:as)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (a:as) = f a >>= \ b ->
                           do bs <- mapM' f as
                              return (b:bs)

--xmapM' f [] = return []
--xmapM' f (a:as) = f a >>= \ b ->
--x                           do bs <- mapM' f as
--x                              return (bs ++ [b])
                            
--mapM' f [] = return []
--mapM' f (a:as) = do b <- f a
--                    bs <- mapM' f as
--                    return (b:bs)
                    
--mapM' f [] = return []
--mapM' f (a:as)
--    = f a >>= \b -> mapM' f as >>= \bs -> return (b:bs)

--mapM' f as = sequence' (map f as)

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x:xs) = do flag <- p x
                       ys <- filterM' p xs
                       if flag then return (x:ys) else return ys

--xfilterM' _ [] = return []
--xfilterM' p (x:xs) = do ys <- filterM' p xs
--x                       if p x then return (x:ys) else return ys
-- Does not type because p x is not bound to a variable (which also unwraps it)
                       
--xfilterM' p (x:xs) = do flag <- p x
--x                       ys <- filterM' p xs
--x                       return (x:ys)

-- Monadic predicate for monadic filter
keepSmall :: Int ->  [Bool]  
keepSmall x  
    | x < 4 = do  
        return True  
    | otherwise = do  
        return False 
  
-- Monadic operator for monadic Left fold        
binsSmallsL :: Int -> Int -> Maybe Int
binsSmallsL acc x
    | x > 9     = Nothing
    | otherwise = Just (acc - x)
    
-- Monadic operator for monadic Right fold        
binsSmallsR :: Int -> Int -> Maybe Int
binsSmallsR x acc
    | x > 9     = Nothing
    | otherwise = Just (acc - x)

-- Recall: return :: a -> m a
--         (>>=)  :: m a -> (a -> m b) -> m b                       
foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (x:xs) = do acc <- a `f` x
                          foldLeftM f acc xs
       
--foldLeftM f a (x:xs) = f a x >>= \ a' ->
--                      do foldLeftM f a' xs

-- fold in the LEFT direction (from right)
-- will "bottom" if provided an infinite list (unlike RIGHT)
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f a [] = a
foldl' f a (x:xs) = let a' = f a x
                    in seq a' $ foldl' f a' xs

-- foldl c z (x:xs) = let z' = z `f` x
--                    in foldl f z' xs

-- foldl (\acc x -> acc + x) 0 [2,8,3,1]                   
-- foldl (+) 1 [1,2,3] => 6

-- Recall: return :: a -> m a
--         (>>=)  :: m a -> (a -> m b) -> m b
foldRightM :: Monad m => (b -> a -> m a) -> a -> [b] -> m a
foldRightM f a [] = return a
foldRightM f a (x:xs) = do acc <- foldRightM f a xs
                           x `f` acc
--xfoldRightM f a (x:xs) = do acc <- x `f`  -- notice how [b] = (x:xs)
--x                           foldRightM f acc xs -- a remains the accumulator
    
foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' f a [] = a
foldr' f a (x:xs) = x `f` foldr' f a xs -- f combines x with tail
                                        -- this combination = accumulator

-- LiftM; takes a regular function, to operate on monads
-- VERY CLEVER demonstration of how we have to work through
-- the chain of monads to get a result.
-- Compare the first, with the currently active.
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= \ a -> return (f a)
--xliftM f m = m >>= \ a -> m >>= \ b -> return (f a) -- delay
--xliftM f m = m >>= \ a -> m >>= \ b -> return (f b) -- delay + lose connection
--xliftM f m = m >> \ a -> return (f a)
--xliftM f m = mapM f [m]
--xliftM f m = return (f m)

--xliftM f m = m >>= \ a -> f a

--liftM f m = do x <- m
--               return (f x)

--test: liftM (map toUpper) getLine               
lsmonads = [Just 3, Just 2, Just 1]

-- do {a ← b; c} is nothing else than b >>= \a → c
-- putChar 'a' >>= \x -> putChar 'b'