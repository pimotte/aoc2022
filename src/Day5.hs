module Day5 where
import Data.List ( transpose )

import Text.Trifecta

import Data.Map (Map, fromList, lookup, insert)


type Crate = Char

type CrateArrangement =  (Map Int [Crate])

data Move = Move 
  { amount :: Integer
  , from :: Int
  , to :: Int
  } deriving (Show, Eq)

parseCrates :: [String] -> Int -> CrateArrangement
parseCrates contents numCrates = 
    let parseLine numCrates line = fmap (\d -> line !! (1 + 4*(d-1))) [1..numCrates]
    in fromList (zip [1..numCrates] (transpose (fmap (parseLine numCrates) contents)))

move :: Parser Move
move = do
    string "move "
    amount <- integer
    string "from "
    from <- integer
    string "to "
    to <- integer
    return (Move amount (fromIntegral from) (fromIntegral to))

parseMove :: String -> Move
parseMove s = r where r = case parseString move mempty s of
                                (Success s) -> s
                                (Failure f) -> Move 1 1 1 

parse :: String -> Int -> (CrateArrangement, [Move])
parse contents numCrates = (
    parseCrates (takeWhile (\s -> s !! 1 /= '1') (lines contents)) numCrates, 
    fmap parseMove (drop 1 (dropWhile (\s -> not (null s) && head s /= 'm') (lines contents)))
    )

executeSingleMove :: CrateArrangement -> Move -> CrateArrangement
executeSingleMove crates move =
    let Just fromPile = Data.Map.lookup (from move) crates
        Just toPile = Data.Map.lookup (to move) crates
       in insert (to move) (head fromPile:toPile) (insert (from move) (drop 1 fromPile) crates)
        

executeMove :: Move -> CrateArrangement -> CrateArrangement
executeMove move crates = 
    case amount move of
        0 -> crates
        _ -> executeMove (Move (amount move - 1) (from move) (to move)) (executeSingleMove crates move) 


message :: CrateArrangement -> Int -> Maybe [Crate]
message crates numCrates = sequence (fmap (\i -> head <$> Data.Map.lookup i crates) [1..numCrates])

solve :: String -> Int -> IO ()
solve contents numCrates = 
    let (crates, moves) = parse contents numCrates
      in do
        print $ message (foldr executeMove crates (reverse moves)) numCrates


day5part1 :: IO String -> IO ()
day5part1 input = do
    contents <- input
    let numCrates = (length (head (lines contents)) + 1) `div` 4
        in solve contents numCrates
