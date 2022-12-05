module Day4 where

import Text.Trifecta


lineParser :: Parser ((Integer, Integer), (Integer, Integer))
lineParser = do
    x1 <- integer
    char '-'
    x2 <- integer
    char ','
    y1 <- integer
    char '-'
    y2 <- integer
    return ((x1, x2), (y1, y2))

parseString' :: String -> ((Integer, Integer), (Integer, Integer))
parseString' s = parsed where (Success parsed) = parseString lineParser mempty s

strictContain :: ((Integer, Integer), (Integer, Integer)) -> Bool
strictContain (p1, p2) = ((fst p1 <= fst p2) && (snd p1 >= snd p2)) 
                           || ((fst p1 >= fst p2) && (snd p1 <= snd p2))

anyOverlap :: ((Integer, Integer), (Integer, Integer)) -> Bool
anyOverlap ((x1, x2), (y1, y2)) = ((x2 >= y1) && (x1 <= y2))

day4part1 :: IO String -> IO ()
day4part1 input = do
    contents <- input
    print $ length (filter strictContain (parseString' <$> lines contents))

day4part2 :: IO String -> IO ()
day4part2 input = do
    contents <- input
    print $ length (filter anyOverlap (parseString' <$> lines contents))