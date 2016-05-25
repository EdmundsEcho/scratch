-- Generate a list next moves given a position
-- next: put the list of possible moves onto the tree
-- the game tree is built using repeated applications
-- of moves.
-- Moves generates the labels for the subtrees of the
-- root.

type Position = Int
data Tree a = Node a [Tree a]
              deriving (Show)

-- higher order recursive helper function
--reptree :: Tree a
reptree f a = Node a (fmap (reptree f) (f a))

gametree :: Position -> Tree Position
gametree p = reptree moves p

gameboard = [1..9]

moves :: Position -> [Position]
moves p = filter (/=p) gameboard

instance Functor Tree where
  fmap f (Node a []) = Node (f a) []
  fmap f (Node a subtree) =
             Node (f a) (fmapsub f subtree)
             where fmapsub f [] = []
                   fmapsub f (tree:subtree) =
                     (fmap f tree):(fmapsub f subtree)

mytree = Node 4
           [Node 3 [Node 1 [Node 2 []]], Node 5 [], Node 3 [], Node 1
                                    [Node 4
                                        [Node 0 []]
                                    , Node 2 []]
           ]
