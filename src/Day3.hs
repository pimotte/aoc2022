module Day3 where

import Data.Set ( foldr, fromList, intersection )
import Data.Char

itemFromLine :: String -> Char
itemFromLine s =
    let (comp1, comp2) = Prelude.splitAt (length s `div` 2) s
    in  Data.Set.foldr (\a b -> a) '*' (intersection (fromList comp1) (fromList comp2))
    
scoreChar :: Char -> Int
scoreChar c 
   | isUpper c = fromEnum c - fromEnum 'A' + 27
   | otherwise = fromEnum c - fromEnum 'a' + 1

groupLines :: [String] -> [[String]]
groupLines [] = []
groupLines xs = (take 3 xs):groupLines (drop 3 xs)

charInCommon :: [String] -> Char
charInCommon s = 
    let a = fmap fromList s
    in Data.Set.foldr (\a b -> a) '*' (intersection (a !! 1) (intersection (head a) (a !! 2)))

day3part1 :: IO String -> IO ()
day3part1 input = do
    contents <- input
    print $ fromEnum 'c' - fromEnum 'a' + 27
    print $ itemFromLine <$> lines contents
    print $ sum (scoreChar . itemFromLine <$> lines contents)

day3part2 :: IO String -> IO ()
day3part2 input = do
    contents <- input
    print $ sum (fmap (scoreChar . charInCommon) (groupLines (lines contents)))