module Day11 where

import Text.Trifecta
import Control.Applicative ((<|>))
import Data.Map (Map, findWithDefault, map, lookup, fromListWith, fromList, elems)
import Data.List (sort)

type Item = Integer

data Atom = Var | Constant Integer deriving (Show, Eq)

data Operation = Add Atom Atom | Mul Atom Atom deriving (Show, Eq)

type Divider = Integer

data Test = Test Divider Integer Integer deriving (Show, Eq)

data Monkey = Monkey 
    { index :: Integer, 
      carrying :: [Item], 
      op :: Operation,
      tester :: Test,
      inspections :: Integer
    } deriving (Show, Eq)

items :: Parser [Item]
items = do
    string "Starting items: "
    items <- integer `sepBy1` (symbol "," >> skipMany space)
    return items

atom :: Parser Atom
atom = do
    skipMany space
    try (string "old" >> return Var) <|> (Constant <$> integer)
   
operation :: Parser Operation
operation = do
    string "Operation: new = "
    atom1 <- atom
    skipMany space
    operand <- oneOf "+*"
    atom2 <- atom
    return (case operand of
                '+' -> Add atom1 atom2
                '*' -> Mul atom1 atom2)
    
test :: Parser Test
test = do 
    string "Test: divisible by "
    divider <- integer
    skipMany space
    string "If true: throw to monkey "
    m1 <- integer
    skipMany space
    string "If false: throw to monkey "
    m2 <- integer
    return (Test divider m1 m2)


monkey :: Parser Monkey
monkey = do
    string "Monkey "
    id <- integer
    char ':'
    skipMany space
    startingItems <- items
    skipMany space
    op <- operation
    skipMany space
    t <- test
    return (Monkey id startingItems op t 0)

value :: Item -> Atom -> Integer
value item (Constant i) = i
value item Var = item

worry :: Item -> Operation -> Integer
worry item (Add atom1 atom2) = ((value item atom1) + (value item atom2)) `div` 3
worry item (Mul atom1 atom2) = (value item atom1) * (value item atom2) `div` 3

testNewWorry :: Integer -> Test -> Integer
testNewWorry worry (Test divider monkeyTrue monkeyFalse) = if worry `mod` divider == 0 then monkeyTrue else monkeyFalse

    

simulateTurn :: Integer -> Map Integer Monkey -> Map Integer [Integer]
simulateTurn monkeyIndex monkeys = 
    let Just monkey = Data.Map.lookup monkeyIndex monkeys
    in fromListWith (++) (fmap (\item -> (testNewWorry (worry item (op monkey)) (tester monkey), [worry item (op monkey)])) (carrying monkey))




simulateRound :: Map Integer Monkey -> Map Integer Monkey  
simulateRound monkeys =
    simulateTurns 0 monkeys where
        simulateTurns monkeyIndex monkeys = 
            let tosses = simulateTurn monkeyIndex monkeys in
            if fromIntegral monkeyIndex == length monkeys then monkeys
            else simulateTurns (monkeyIndex + 1) ((Data.Map.map (\monkey -> 
                if index monkey == monkeyIndex then
                    Monkey (index monkey) [] (op monkey) (tester monkey) (fromIntegral (length (carrying monkey)) + inspections monkey)
                else
                    Monkey (index monkey) (carrying monkey ++ reverse (findWithDefault [] (index monkey) tosses)) (op monkey) (tester monkey) (inspections monkey)
                    )) monkeys)
                        
monkeyItems :: Map Integer Monkey -> [String]
monkeyItems monkeys = fmap (\m -> "Monkey " ++ (show (index m)) ++ " : " ++ (show (carrying m))) (elems monkeys)

optimisticParse :: String -> [Monkey]
optimisticParse contents = s where (Success s) = (parseString (many monkey) mempty contents)

simulateRounds monkeyMap n = if n == 0 then monkeyMap else simulateRounds (simulateRound monkeyMap) (n-1)

day11part1 :: IO String -> IO ()
day11part1 input = do
    contents <- input
    print $ parseString (many monkey) mempty contents
    -- let monkeyMap = fromList (fmap (\m -> (index m, m)) (optimisticParse contents))
    --    in mapM_ putStrLn (monkeyItems (simulateRound (monkeyMap)))
    let monkeyMap = fromList (fmap (\m -> (index m, m)) (optimisticParse contents))
       in mapM_ putStrLn (monkeyItems (simulateRounds (monkeyMap) 20))
    let monkeyMap = fromList (fmap (\m -> (index m, m)) (optimisticParse contents))
       in print $ product (take 2 (reverse (sort (fmap (\m -> inspections m) (elems (simulateRounds (monkeyMap) 20))))))