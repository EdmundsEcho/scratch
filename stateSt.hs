{-# LANGUAGE InstanceSigs #-}

f :: a -> b
f = undefined

stateA = St $ \s -> (0, s)
stateF = St $ \s -> ((5+), s)

newtype St s a = St { runSt :: s -> (a, s) }

instance Functor (St s) where
    fmap :: (a -> b) -> St s a -> St s b
    fmap f (St sa) = St $ \s -> let (a, s') = sa s
                                in  (f a, s')
                           
instance Applicative (St s) where
    pure a = St $ \s -> (a, s)
    (St sf) <*> (St sa) = St $ \s -> let (f,s')  = sf s
                                         (a,s'') = sa s'
                                     in (f a, s'')
        
instance Monad (St s) where
    return = pure
    (>>=) :: St s a -> (a -> St s b) -> St s b
    (St sa) >>= fm = St $ \s -> let (a, s') = sa s
                                in  (runSt (fm a) s')