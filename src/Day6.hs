module Day6 where

import Data.Set (Set, size, fromList)
import Data.List (find, findIndex, zip4)


markerIndex s = fmap (+4) (findIndex (\(a, b, c, d) -> size (fromList [a, b, c, d]) == 4) 
                   (zip4 s (drop 1 s) (drop 2 s) (drop 3 s)))

messageMarkerIndex s = fmap (+14) (find (\i -> size (fromList (take 14 (drop i s))) == 14) [0..length s])

day6part1 :: IO String -> IO ()
day6part1 input = do
    contents <- input
    print $ markerIndex contents

day6part2 :: IO String -> IO ()
day6part2 input = do
    contents <- input
    print $ messageMarkerIndex contents