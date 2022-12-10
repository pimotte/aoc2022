module Day8 where
import Data.Char (digitToInt)
import Data.Set (Set, insert, empty, union, size)

parseGrid :: [String] -> [[Int]]
parseGrid strs = (fmap . fmap) (digitToInt) strs

dimensions :: [String] -> (Int, Int)
dimensions strs = (length (head strs), length strs)


partResults :: [[Int]] -> (Int, Int) -> [Set (Int, Int)]
partResults grid dims  =
            [visibleTreesFrom (0, sy) (\(a, b) -> (a+1, b)) dims grid | sy <- [0..snd dims]] ++
            [visibleTreesFrom (sx, 0) (\(a, b) -> (a, b+1)) dims grid | sx <- [0..fst dims]] ++
            [visibleTreesFrom (fst dims - 1, sy) (\(a, b) -> (a-1, b)) dims grid | sy <- [0..snd dims]] ++
            [visibleTreesFrom (sx, snd dims - 1) (\(a, b) -> (a, b-1)) dims grid | sx <- [0..fst dims]]

solve :: [[Int]] -> (Int, Int) -> Int
solve grid dims = size (foldr union empty (partResults grid dims)) where 
          



validPos :: (Int, Int) -> (Int, Int) -> Bool
validPos pos dims = fst pos >= 0 && snd pos >= 0 && fst pos < fst dims && snd pos < snd dims

visibleTreesFrom :: (Int, Int) -> ((Int, Int) -> (Int, Int)) -> (Int, Int) -> [[Int]] -> Set (Int, Int)
visibleTreesFrom startPos delta dims grid = 
    go (-1) startPos where
        go highest curPos = if validPos curPos dims then 
                if ((grid !! snd curPos) !! fst curPos) > highest then
                    insert curPos (go ((grid !! snd curPos) !! fst curPos) (delta curPos))
                else 
                    go highest (delta curPos)
            else empty

sightLineLength :: (Int, Int) -> ((Int, Int) -> (Int, Int)) -> (Int, Int) -> [[Int]] -> Int
sightLineLength startPos delta dims grid =
    let target = ((grid !! snd startPos) !! fst startPos)
    in go target (delta startPos) 1 where
        go target pos dist = if validPos pos dims then
                            if ((grid !! snd pos) !! fst pos) >= target then
                                dist
                            else go target (delta pos) (dist + 1)
                      else (dist-1) 

scoreTree :: (Int, Int) -> [[Int]] -> (Int, Int) -> Int
scoreTree dims grid pos = (sightLineLength pos (\(a, b) -> (a+1, b)) dims grid) *
                     (sightLineLength pos (\(a, b) -> (a, b+1)) dims grid) *
                      (sightLineLength pos (\(a, b) -> (a-1, b)) dims grid) *
                       (sightLineLength pos (\(a, b) -> (a, b-1)) dims grid)

bestTreeScore :: [[Int]] -> (Int, Int) -> Int
bestTreeScore grid dims = maximum [scoreTree dims grid (x,y) | x <- [1..fst dims-2], y <- [1..snd dims-2]] where
    


day8part1 :: IO String -> IO ()
day8part1 input = do
    contents <- input
    -- print $ dimensions (lines contents)
    -- print $ (foldr union empty (partResults (parseGrid (lines contents)) (dimensions (lines contents))))
    print $ solve (parseGrid (lines contents)) (dimensions (lines contents))

day8part2 :: IO String -> IO ()
day8part2 input = do
    contents <- input
    -- print $ (sightLineLength (1, 2) (\(a, b) -> (a, b-1)) (dimensions (lines contents)) (parseGrid (lines contents))) 
    -- print $ [(x, y, (scoreTree (dimensions (lines contents)) (parseGrid (lines contents)) (x,y))) | x <- [1..fst (dimensions (lines contents)) -2], y <- [1..snd (dimensions (lines contents))-2]]
    print $ bestTreeScore (parseGrid (lines contents)) (dimensions (lines contents))