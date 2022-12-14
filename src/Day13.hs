module Day13 where
import Data.List.Split ( chunksOf )
import Text.Trifecta

import Control.Applicative
import Data.List (findIndex, elemIndex, sort)

data NestedList = NestedList [NestedList] | Atom Integer deriving (Show, Eq)

atom :: Parser NestedList
atom = Atom <$> integer 

nestedList :: Parser NestedList
nestedList = try atom <|> NestedList <$> (char '[' *> (nestedList `sepBy` char ',') <* char ']')


instance Ord NestedList where
  compare (Atom i) (Atom j) = compare i j
  compare (Atom i) lj = compare (NestedList [Atom i]) lj
  compare li (Atom j) = compare li (NestedList [Atom j])
  compare (NestedList li) (NestedList lj) = compare li lj
    


splitAtnewLines :: [String] -> [[String]]
splitAtnewLines = chunksOf 3

optimisticParse :: String -> NestedList
optimisticParse s = r where (Success r) = parseString nestedList mempty s

isCorrect :: [String] -> Bool
isCorrect pair = (optimisticParse (head pair)) <= (optimisticParse (pair !! 1)) 

obtainCorrectIndices :: [[String]] -> [Int]
obtainCorrectIndices chunks = filter (\i -> isCorrect (chunks !! (i-1))) [1..length chunks]

day13part1 :: IO String -> IO ()
day13part1 input = do
    contents <- input
    print $ optimisticParse "[1,1,3,1,1]"
    print $ optimisticParse "[1,1,5,1,1]"
    print $ obtainCorrectIndices (splitAtnewLines (lines contents))
    print $ sum (obtainCorrectIndices (splitAtnewLines (lines contents)))

optimisticElemIndex needle haystack = r where (Just r) = elemIndex needle haystack

findKey :: String -> Int
findKey contents = 
    (1 + optimisticElemIndex (optimisticParse "[[2]]") sList) * (1 + optimisticElemIndex (optimisticParse "[[6]]") sList)
    where sList = sort (fmap optimisticParse (filter (\s -> length s > 0) (lines contents)) ++ [optimisticParse "[[2]]", optimisticParse "[[6]]"])

day13part2 :: IO String -> IO ()
day13part2 input = do
    contents <- input
    print $ findKey contents