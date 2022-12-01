module Day1 where

import Text.Trifecta
import Control.Applicative
import Data.Foldable (maximumBy)
import Data.Function
import Data.List (sortBy)

type Elf = [Integer]

strToElfs :: String -> [Elf]
strToElfs s = foldr f [] (lines s) where
                    f "" elfs = []:elfs
                    f s (e:es) = (read s:e):es
                    f s  [] = [[read s]]
day1 :: IO String -> IO ()
day1 input = do
    contents <- input
    print $ sum (maximumBy (compare `on` sum) (strToElfs contents))

day1part2 :: IO String -> IO ()
day1part2 input = do
    contents <- input
    print $  fmap sum (sortBy (compare `on` sum) (strToElfs contents))
    print $ head (l contents) + (l contents) !! 1 + (l contents) !! 2 where 
                 l contents = fmap sum (reverse (sortBy (compare `on` sum) (strToElfs contents)))