module Day14 where
import Text.Trifecta
import Data.Set ( Set, empty, union, fromList, member, insert )
import Data.Foldable (maximumBy)
import Data.Function (on)
import Debug.Trace (trace)

rockPath :: Parser [(Integer, Integer)]
rockPath = ((,) <$> integer <*> (char ',' >> integer)) `sepBy` (string "-> ")

optimisticParse :: String -> [(Integer, Integer)]
optimisticParse l = r where (Success r) = parseString rockPath mempty l

rockSet :: [(Integer, Integer)] -> Set (Integer, Integer)
rockSet endpoints = foldr f empty (zip endpoints (tail endpoints))
          where f ((x1, y1), (x2, y2)) curSet = union curSet (fromList [(x, y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]])

allRocks :: [String] -> Set (Integer, Integer)
allRocks paths = foldr union empty (fmap (rockSet . optimisticParse) paths)

-- rockBottom :: Set (Integer, Integer) -> (Integer, Integer) -> Bool
-- rockBottom rocks (x,y) = (fromList [(x-1, y+1), (x, y+1), (x+1, y+1)]) `isSubsetOf` rocks

rockBottom :: Set (Integer, Integer) -> Integer
rockBottom set = snd (maximumBy (compare `on` snd) set)

simulateStep :: Integer -> Set (Integer, Integer) -> (Integer, Integer) -> Integer
simulateStep minimum rocks (x,y) 
    | minimum <= y = 0
    | not (member (x, y+1) rocks) = simulateStep minimum rocks (x, y+1)
    | not (member (x-1, y+1) rocks) = simulateStep minimum rocks (x-1, y+1)
    | not (member (x+1, y+1) rocks) = simulateStep minimum rocks (x+1, y+1)
    | (x,y) == (500, 0) = 1
    | otherwise = simulateStep minimum (insert (x, y) rocks) (500, 0) + 1

-- simulateStep :: Integer -> Set (Integer, Integer) -> (Integer, Integer) -> Integer
-- simulateStep minimum rocks (x,y) 
--     | trace ("Position: " ++ show (x,y) ++ " Rocks: " ++ show rocks ++ show (not (member (x, y+1) rocks))) minimum <= y = 0
--     | not (member (x, y+1) rocks) = trace "reached down" simulateStep minimum rocks (x, y+1)
--     | not (member (x-1, y+1) rocks) = trace "reached left" simulateStep minimum rocks (x-1, y+1)
--     | not (member (x+1, y+1) rocks) = trace "reached right" simulateStep minimum rocks (x+1, y+1)
--     | otherwise = trace "reached +1" simulateStep minimum (insert (x, y) rocks) (500, 0) + 1

    -- if minimum <= y then 0 else
    --           if member (x, y+1) rocks then
    --             if member (x-1, y+1) rocks then
    --                 if member (x+1, y+1) rocks then
    --                     trace "reached +1" simulateStep minimum (insert (x, y) rocks) (500, 0) + 1
    --                 else trace "reached right" simulateStep minimum rocks (x+1, y+1)
    --             else trace "reached left" simulateStep minimum rocks (x-1, y+1)
    --           else trace "reached down" simulateStep minimum rocks (x, y+1)
numberOfGrains :: Integer -> Set (Integer, Integer) -> Integer
numberOfGrains minimum rocks = simulateStep minimum rocks (500, 0) 
        

day14part1 :: IO String -> IO ()
day14part1 input = do
    contents <- input
    -- print $ allRocks (["498,4 -> 498,6 -> 496,6", "503,4 -> 502,4 -> 502,9 -> 494,9"])
    -- print $ allRocks (lines contents)
    -- print $ numberOfGrains 3 (allRocks (["499,3 -> 501,3"]))
    -- print $ rockBottom (allRocks (lines contents))
    print $ numberOfGrains (rockBottom (allRocks (lines contents))) (allRocks (lines contents))

day14part2 :: IO String -> IO ()
day14part2 input = do
    contents <- input
    print $ rockBottom (allRocks (lines contents))
    print $ "-10000," ++ show (rockBottom (allRocks (lines contents))+2) ++ "-> 10000," ++ show (rockBottom (allRocks (lines contents))+2)
    print $ numberOfGrains (rockBottom (allRocks (lines contents))+3) (allRocks (("-10000," ++ show (rockBottom (allRocks (lines contents))+2) ++ "-> 10000," ++ show (rockBottom (allRocks (lines contents))+2)):lines contents))