{-# LANGUAGE ScopedTypeVariables #-}

module Hole where

import Control.Monad()

data Hole = Hole
data Hole1 a = Hole1
hole = undefined

--compose :: forall a b c. (b -> c) -> (a -> b) -> a -> c
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
   -- where
   --     _ = f :: b -> c
   --     _ = g :: a -> b
   --     _ = x :: a

--apply :: forall m a b. Monad m =>  m (a -> b) -> m a -> m b
apply :: Monad m =>  m (a -> b) -> m a -> m b
--apply mf ma = mf >>= \f -> ma >>= \x -> return (f x)
apply mf ma = do
   f <- mf
   x <- ma
   return (f x)
   --  where
   --    _ = mf :: m (a -> b)
   --    _ = ma :: m a
   --    _ = (>>=) :: forall h. m h -> (h -> m b) -> m b
   --    _ = (mf >>=) :: ((a -> b) -> m b) -> m b
   --    _ = (ma >>=) :: (a -> m b) -> m b
   --    k f = ma >>= r
   --       where
   --          _ = return :: b -> m b
   --          _ = f :: a -> b
   --          r x = return (f x) :: m b
   --             where
   --                _ = x :: a
   --                _ = f x :: b
   --                _ = return (f x) :: m b
      --   _ = (mf >>=) :: forall h. ((a ->b) -> m b) -> m b
      --   _ = (ma >>=) :: forall h. (a -> m b) -> m b
      --   _ = mf :: m (a -> b)
      --   _ = ma :: m a
      --   k f = ma >>= r
      --     where
      --        _ = f :: a -> b
      --        r x = return (f x) :: m b
      --          where
      --              _ = x :: a
      --              _ = f x :: b
      --              _ = return (f x) :: m b

filterM' :: forall m a. Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' p xs = foldr f (return []) xs :: m [a]
   where
   --    _ = p :: a -> m Bool
   --    _ = xs :: [a]
      f x r = do
         c <- p x
         if c then do
            ys <- r
            return (x:ys)
         else
            r

      -- f x r = p x >>= \c -> if c then r >>= \ys -> return (x:ys) else r
      --
      -- f x r = p x >>= k
      --    where
      --       _ = x :: a
      --       _ = r :: m [a]
      --       k c = if c then r >>= j else r :: m [a]
      --          where
      --             _ = c :: Bool
      --             j ys = return (x:ys) :: m [a]
      --                where
      --                   _ = ys :: [a]
      --                   _ = (x:xs) :: [a]
      --                   _ = return (x:xs) :: m [a]
