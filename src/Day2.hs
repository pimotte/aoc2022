module Day2 where

data Move = Rock | Paper | Scissors deriving (Show, Eq)

data Turn = Turn Move Move deriving (Show, Eq)

data Goal = Win | Draw | Loss deriving (Show, Eq)

data Code = Code Move Goal deriving (Show, Eq)

turnFromLine :: String -> Turn
turnFromLine s = 
    let they = case head s of
                 'A' -> Rock
                 'B' -> Paper
                 'C' -> Scissors
        us = case s !! 2 of
                 'X' -> Rock
                 'Y' -> Paper
                 'Z' -> Scissors
    in Turn they us

turnsFromString :: String -> [Turn]
turnsFromString s = fmap turnFromLine (lines s) 

codeFromLine :: String -> Code
codeFromLine s = 
    let they = case head s of
                 'A' -> Rock
                 'B' -> Paper
                 'C' -> Scissors
        goal = case s !! 2 of
                 'X' -> Loss
                 'Y' -> Draw
                 'Z' -> Win
    in Code they goal

codesFromString :: String -> [Code]
codesFromString s = fmap codeFromLine (lines s) 

moveFromCode :: Code -> Move
moveFromCode c =   
    case c of
        (Code Rock Win) -> Paper
        (Code Rock Draw) -> Rock
        (Code Rock Loss) -> Scissors
        (Code Paper Win) -> Scissors
        (Code Paper Draw) -> Paper
        (Code Paper Loss) -> Rock
        (Code Scissors Win) -> Rock
        (Code Scissors Draw) -> Scissors
        (Code Scissors Loss) -> Paper

turnFromCode :: Code -> Turn
turnFromCode c@(Code they goal) = Turn they (moveFromCode c)


scoreTurn :: Turn -> Integer
scoreTurn (Turn they us) = 
    let base = case us of
                Rock -> 1
                Paper -> 2
                Scissors -> 3
        winOrLoss = case (they, us) of
                (Rock, Rock) -> 3
                (Paper, Paper) -> 3
                (Scissors, Scissors) -> 3
                (Rock, Paper) -> 6
                (Paper, Scissors) -> 6
                (Scissors, Rock) -> 6
                _ -> 0
    in base + winOrLoss

day2part1 :: IO String -> IO ()
day2part1 input = do
    contents <- input
    print $ sum (fmap scoreTurn (turnsFromString contents))

day2part2 :: IO String -> IO ()
day2part2 input = do
    contents <- input
    print $ sum (fmap (scoreTurn . turnFromCode) (codesFromString contents))
