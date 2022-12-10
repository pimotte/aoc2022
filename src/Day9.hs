module Day9 where
import Text.Trifecta
import Data.Set


data Direction = Up | Down | Rig | Lef | UR | UL | DR | DL | Nowhere deriving (Show, Eq)

data Move = Move {
    dir :: Direction,
    amount :: Integer
} deriving (Show, Eq)

move :: Parser Move
move = do
    c <- oneOf "UDRL"
    char ' '
    d <- integer
    return (Move (case c of
                   'U' -> Up
                   'D' -> Down
                   'L' -> Lef
                   'R' -> Rig
                   ) d)

parseMoves :: String -> [Move]
parseMoves contents = fmap parseMove (lines contents) where
    parseMove s = r where (Success r) = parseString move mempty s

moveHeadSingle :: (Integer, Integer) -> Move -> (Integer, Integer)
moveHeadSingle (x, y) (Move dir _) = 
    case dir of
        Up -> (x, y+1)
        Down -> (x, y-1)
        Rig -> (x+1, y)
        Lef -> (x-1, y)
        UR -> (x+1, y+1)
        UL -> (x-1, y+1)
        DR -> (x+1, y-1)
        DL -> (x-1, y-1)
        Nowhere -> (x, y)

tailFollow :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
tailFollow (xh, yh) (xt, yt) =
    if abs (xh - xt) >= 2 || abs (yh - yt) >= 2 then
             (xt + signum (xh - xt), yt + signum (yh - yt))
        else (xt, yt)

oldTailToTailMove :: (Integer, Integer) -> (Integer, Integer) -> Move
oldTailToTailMove (ox, oy) (tx, ty) =
    case (ox - tx) of
        -1 -> case (oy - ty) of
                 -1 -> Move DL 1
                 0 -> Move Lef 1
                 1 -> Move UL 1
        0 -> case (oy - ty) of
                 -1 -> Move Down 1
                 0 -> Move Nowhere 1
                 1 -> Move Up 1
        1 -> case (oy - ty) of
                 -1 -> Move DR 1
                 0 -> Move Rig 1
                 1 -> Move UR 1

simulateMovesToMoves :: [Move] -> [Move]
simulateMovesToMoves moves = 
    go (0, 0) (0, 0) moves [] where
        go _ _ [] targetMoves = reverse targetMoves
        go h t moves targetMoves =
            let newHead = moveHeadSingle h (head moves)
            in
                let newTail = tailFollow newHead t
                in if (amount (head moves)) == 1 then
                       go newHead newTail (tail moves) ((oldTailToTailMove t newTail):targetMoves)
                   else go newHead newTail ((Move (dir (head moves)) (amount (head moves) - 1)):tail moves) ((oldTailToTailMove t newTail):targetMoves)


simulateMoves :: [Move] -> Int
simulateMoves moves = 
    size (go (0, 0) (0, 0) moves (singleton (0, 0))) where
        go _ _ [] visited = visited
        go h t moves visited =
            let newHead = moveHeadSingle h (head moves)
            in
                let newTail = tailFollow newHead t
                in if (amount (head moves)) == 1 then
                       go newHead newTail (tail moves) (insert newTail visited)
                   else go newHead newTail ((Move (dir (head moves)) (amount (head moves) - 1)):tail moves) (insert newTail visited)
                 

day9part2 :: IO String -> IO ()
day9part2 input = do
    contents <- input
    -- print $ tailFollow (1, 1) (0, 0)
    print $ parseMoves contents
    -- print $ simulateMovesToMoves (parseMoves contents)
    print $ simulateMoves (( simulateMovesToMoves . simulateMovesToMoves
                             . simulateMovesToMoves . simulateMovesToMoves . simulateMovesToMoves
                             . simulateMovesToMoves . simulateMovesToMoves . simulateMovesToMoves) (parseMoves contents))
