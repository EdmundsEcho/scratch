
import Prelude hiding (min,max)

-- Why FP matters
-- John Hughes
{-
alpha beta heuristic
types

position
-}

type Position = Int
type Value = Int
type GameTree = [[Position]]
type List a = Nil | List a
type Tree a = Node a (List (Tree a))

instance Functor List where
  fmap f = foldl (cons . f) Nil

moves :: Position -> [Positions]
moves p = undefined -- = free positions

-- repeated applications of moves
reptree f a = Node a (fmap (reptree f) (f a))

-- tree of moves
gametree :: Position -> GameTree
gametree p = reptree moves p

-- evaluates the value of a move
static :: Position -> Value
static = undefined

maximise (Node n Nil) = n
maximise (Node n sub) = max (fmap minimise sub)
minimise (Node n Nil) = n
minimise (Node n sub) = min (fmap maximise sub)

evaluate = maximise . maptree static . prune 5 . gametree

prune 0 (Node a x) = Node a Nil
prune n (Node a x) = Node a (fmap(prune (n-1)) x)

maximise = max . maximise'

maximise' (Node n Nil) = cons n Nil
maximise' (Node n l) = fmap minimise l
                     = fmap (min . minimise') l
                     = map min (map minimise' l)
                     = mapmin (fmap minimise' l)
          where mapmin = fmap min

mapmin (cons nums rest) =
          cons (min nums) (omit (min nums) rest)

omit pot Nil = Nil
omit pot (cons nums rest) =
         | minleq nums pot -> omit pot rest
         | otherwise -> cons (min nums) (omit (min nums) rest)
