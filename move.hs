--move.hs--

type Pos = (Int,Int)

data Move = North | South | East | West
    deriving Show

move :: Move -> Pos -> Pos

move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos

moves [] p = p
moves (m:ms) p = moves ms (move m p) 