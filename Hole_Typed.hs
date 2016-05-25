{-# LANGUAGE ScopedTypeVariables #-}

module Hole_Typed where

data Hole = Hole
data Hole1 a = Hole1 a
hole = undefined

compose :: forall a b c. (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
  where
    _ = f :: (b -> c)
    _ = g :: (a -> b)
    _ = x :: a

apply :: forall m a b. Monad m =>  m (a -> b) -> m a -> m b
apply mf ma = mf >>= k
  where
    _ = return :: b -> m b
    _ = mf :: m (a -> b)
    _ = ma :: m a
    _ = (>>=) :: forall h. m h -> (h -> m b) -> m b
    _ = (mf >>=) :: ((a -> b) -> m b) -> m b
    _ = (ma >>=) :: (a -> m b) -> m b
    k f = ma >>= r :: m b
      where
        _ = k :: (a -> b) -> m b
        _ = f :: a -> b
        r x = return (f x)  :: m b
          where
            _ = x :: a
            _ = f x :: b
            _ = return (f x) :: m b

apply' :: forall m a b. Monad m => m (a -> b) -> m a -> m b
apply' mf ma = mf >>= k
  where
    _ = return :: b -> m b
    _ = mf :: m (a -> b)
    _ = ma :: m a
    _ = (>>=) :: forall h. m h -> (h -> m b) -> m b
    _ = (mf >>=) :: ((a -> b) -> m b) -> m b
    _ = (ma >>=) :: (a -> m b) -> m b
    k z = ma >>= f :: m b
      where
        _ = z :: a -> b
        _ = f :: a -> m b
        f x = return (z x) :: m b
          where
            _ = x :: a

-- apply' mf ma = mf >>= \z -> ma >>= \x -> return (z x)
filterM' :: forall m a. Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' p xs = foldr fn (return []) xs :: m [a]
  where
    _ = p :: (a -> m Bool)
    _ = xs :: [a]
    _ = fn :: a -> m [a] -> m [a]
    fn x r = p x >>= k :: m [a]
      where
        _ = x :: a
        _ = r :: m [a]
        _ = k :: Bool -> m [a]
        k c = if c then r >>= j else r :: m [a]
          where
            _ = c :: Bool
            _ = j :: [a] -> m [a]
            j ys = return (x:ys) :: m [a]
              where
                _ = ys :: [a]
                _ = (x:ys) :: [a]
                _ = return (x:ys) :: m [a]
