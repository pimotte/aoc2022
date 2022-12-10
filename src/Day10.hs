module Day10 where
import Text.Trifecta
import Control.Applicative ((<|>))
import Data.List.Split (chunksOf)


data Instruction = Noop | Addx Integer deriving (Show, Eq)

noop :: Parser Instruction
noop = string "noop" >> return Noop

addx :: Parser Instruction
addx = do 
    string "addx "
    i <- integer
    return (Addx i)


instruction :: Parser Instruction
instruction = try noop <|> addx

parseInstructions :: String -> [Instruction]
parseInstructions str = concatMap f (lines str) where
    f line = let (Success s) = parseString instruction mempty line in
                 case s of 
                    Noop -> [Noop]
                    addx -> [Noop, addx]

simulate :: [Instruction] -> Integer
simulate ins  = 
    go ins 1 where
        go [] res = res
        go (i:ins) res = case i of
                        Noop -> go ins res
                        (Addx i) -> go ins res + i

getSignalStrengths :: [Instruction] -> Integer
getSignalStrengths ins = sum (fmap (\i -> (20+i*40) * (simulate (take (20+(fromIntegral i)*40 - 1) ins))) [0..5])

drawCrt :: [Instruction] -> String
drawCrt ins  = 
    go ins 1 0 "" where
        go [] _ _ res = reverse res
        go (i:ins) x cycle res = case i of
                        Noop -> go ins x (cycle+1) ((charToDraw x cycle):res)
                        (Addx i) -> go ins (x + i) (cycle +1) ((charToDraw (x + i) cycle):res)
                        where charToDraw x cycle = if abs ((cycle `mod` 40) + 1 - x) < 2 then '#' else '.' 

splitCrt :: String -> [String]
splitCrt str = (chunksOf 40 str)

day10part1 :: IO String -> IO ()
day10part1 input = do
    contents <- input
    print $ parseInstructions contents
    print $ simulate (take 179 (parseInstructions contents))
    print $ getSignalStrengths (parseInstructions contents)

day10part2 :: IO String -> IO ()
day10part2 input = do
    contents <- input
    mapM_ putStrLn (splitCrt (drawCrt (parseInstructions contents)))