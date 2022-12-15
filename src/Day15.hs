module Day15 where

import Text.Trifecta.Parser 
import Text.Trifecta
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Data.List (sortBy)
import Day11 (Monkey(op))
import Data.Set ( fromList, size )
import Debug.Trace (trace)

data Sensor = Sensor {
    location :: (Integer, Integer),
    beacon :: (Integer, Integer)
} deriving (Show, Eq)

sensorRange :: Sensor -> Integer
sensorRange (Sensor (xl, yl) (xb, yb)) = abs (xl - xb) + abs (yl - yb)

blockedRangeAtHeight :: Integer -> Sensor -> Maybe (Integer, Integer)
blockedRangeAtHeight height sensor = 
    let heightDiff = abs (height - snd (location sensor))
    in if heightDiff <= sensorRange sensor then 
        Just (fst (location sensor) - (sensorRange sensor - heightDiff), 
              fst (location sensor) + (sensorRange sensor - heightDiff))
       else Nothing

amountOfObjects height sensors = 
    let locations = [(x, y) | (x, y) <- fmap location sensors, y == height] ++ [(x, y) | (x, y) <- fmap beacon sensors, y == height]
    in size (fromList locations)

countBlockedAtHeight :: Integer -> [Sensor] -> Integer
countBlockedAtHeight height sensors = 
    let ranges = sortBy (compare `on` fst) (mapMaybe (blockedRangeAtHeight height) sensors)
    in count ranges (fst (head ranges) - 1) where
        count [] _ = fromIntegral (- amountOfObjects height sensors)
        count (r:rs) countedTo
          -- countedTo = -4, r = (-3, 7)   
          | countedTo < fst r = (1 + snd r - fst r) + count rs (snd r)
          | countedTo < snd r = (snd r - countedTo) + count rs (snd r)
          | otherwise = count rs countedTo

-- countBlockedAtHeight' :: Integer -> Integer -> [Sensor] -> Integer
-- countBlockedAtHeight' cap height sensors = 
--     let ranges = sortBy (compare `on` fst) (mapMaybe (blockedRangeAtHeight height) sensors)
--     in count ranges (-1) where
--         count [] _ = fromIntegral (- amountOfObjects height sensors)
--         count (r:rs) countedTo
--           -- countedTo = -4, r = (-3, 7)
--           | cap < snd r = (cap - (max (fst r) (countedTo-1)) + 1)
--           | countedTo < fst r = ((1 + snd r - fst r) + count rs (snd r))
--           | countedTo < snd r = (snd r - countedTo) + count rs (snd r)
--           | otherwise = count rs countedTo

countBlockedAtHeight' :: Integer -> Integer -> [Sensor] -> Integer
countBlockedAtHeight' cap height sensors = 
    let ranges = sortBy (compare `on` fst) (mapMaybe (blockedRangeAtHeight height) sensors)
    in count ranges (-1) where
        count [] _ = fromIntegral (- amountOfObjects height sensors)
        count (r:rs) countedTo
          -- countedTo = -4, r = (-3, 7)
          | cap < snd r = trace (if fst r - countedTo > 1 then "height: " ++ show (height) ++ "x: " ++ (show (countedTo + 1)) else "") (cap - (max (fst r) (countedTo-1)) + 1)
          | countedTo < fst r = trace (if fst r - countedTo > 1 then "height: " ++ show (height) ++ "x: " ++ (show (countedTo + 1)) else "")  ((1 + snd r - fst r) + count rs (snd r))
          | countedTo < snd r = (snd r - countedTo) + count rs (snd r)
          | otherwise = count rs countedTo    

sensorParser :: Parser Sensor
sensorParser = do
    string "Sensor at x="
    xl <- integer
    string ", y="
    yl <- integer
    string ": closest beacon is at x="
    xb <- integer
    string ", y="
    yb <- integer
    return (Sensor (xl, yl) (xb, yb))

optimisticParse :: String -> Sensor
optimisticParse s = r where (Success r) = parseString sensorParser mempty s

day15part1 :: IO String -> IO ()
day15part1 input = do
    contents <- input
    -- print $ parseString sensorParser mempty (lines contents !! 6)
    -- print $ mapMaybe (blockedRangeAtHeight 10) (fmap optimisticParse ([lines contents !! 6]))
    -- print $ countBlockedAtHeight (-1) (fmap optimisticParse (["Sensor at x=8, y=7: closest beacon is at x=2, y=10"]))
    print $ fmap optimisticParse (lines contents)
    print $ sortBy (compare `on` fst) (mapMaybe (blockedRangeAtHeight 2000000) (fmap optimisticParse (lines contents)))
    print $ countBlockedAtHeight 2000000 (fmap optimisticParse (lines contents))

day15part2 :: IO String -> IO ()
day15part2 input = do
    contents <- input
    print $ filter (\h -> 4000000 == countBlockedAtHeight' 4000000 h (fmap optimisticParse (lines contents))) [2836448]