type Board = [ (Int,Int),Player ]
type Player = X | O

emptyBoard :: Board
emptyBoard = []

occupied :: Board -> [Position]
occupied = undefined

isMarked :: Board -> Position -> Bool
isMarked = undefined

isEmpty :: Board -> Position -> Bool
isEmpty p = not (isMarked p)

available :: Board -> [Position]
available = undefined

moves :: Board -> Player -> [Board]
moves = undefined
