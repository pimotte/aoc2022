module Day7 where
import Text.Trifecta (parseString, string, char, noneOf, many, notChar, try, integer, manyTill)
import Text.Trifecta.Result
import Text.Trifecta.Parser
import Text.Trifecta.Combinators
import Control.Applicative
import Data.Tree
import Debug.Trace
import Data.Monoid
import GHC.IO.Handle (NewlineMode(inputNL))
import Data.Foldable (maximumBy)
import Data.List (minimumBy)
import Data.Function (on)

data File = File {
    name :: String,
    size :: Integer
} deriving (Show, Eq)

data Command = Cd String | Ls [File] deriving (Show, Eq)


debug = False

dir :: Parser File
dir = do
       string "dir "
       arg <- (many (notChar '\n'))
       char '\n'
       return (File arg 0)

realFile :: Parser File
realFile = do
            size <- integer
            name <- (many (notChar '\n'))
            char '\n'
            return (File name size)

file :: Parser File
file = try dir <|> try realFile


cd :: Parser Command
cd = do
      string "cd "
      arg <- (many (notChar '\n'))
      char '\n'
      return (Cd arg)

ls :: Parser Command
ls = do 
      string "ls\n"
      files <- (many file)
      return (Ls files)


command :: Parser Command
command = do
    try (string "$ ")
    res <- try cd <|> ls
    return res



parseInput :: String -> [Command]
parseInput str =
    case parseString (many command) mempty str of
          (Success s) -> s
          (Failure s) -> []

replaceInTree :: [String] -> [File] -> Tree File -> Tree File
-- replaceInTree [] files tree = Node (rootLabel tree) (fmap newSubtree files) where
--     newSubtree file = Node file []
replaceInTree [dir] files tree = Node (rootLabel tree) (if dir == name (rootLabel tree) then fmap newSubtree files else subForest tree) where
    newSubtree file = Node file []
replaceInTree (dir:dirs) files tree = Node (rootLabel tree) (if dir == name (rootLabel tree) then fmap (replaceInTree dirs files) (subForest tree) else subForest tree) 

                                            

commandsToTree :: [Command] -> Tree File
commandsToTree commands = go ["/"] commands (Node (File "/" 0) []) where
                                go _ [] tree = tree
                                go curDir ((Cd arg):cs) tree = 
                                    case arg of
                                        ".." -> trace (if debug then "Exec cd .." ++ show tree else "") go (tail curDir) cs tree
                                        "/" -> go ["/"] cs tree
                                        s -> go (s:curDir) cs tree
                                go curDir ((Ls files):cs) tree = trace (if debug then "Exec ls in" ++ show curDir ++ "files: " ++ show files ++ "tree: " ++ show tree else "") (go curDir cs (replaceInTree (reverse curDir) files tree))
            
calcSize :: Tree File -> Integer
calcSize t = size (rootLabel t) + sum (fmap calcSize (subForest t))

sumIf :: Tree File -> Integer
sumIf t = let cur = if calcSize t <= 100000 && size (rootLabel t) == 0 then calcSize t else 0
          in cur + sum (fmap sumIf (subForest t))

totalSpace = 70000000

neededSpace = 30000000

neededDelete t = neededSpace - (totalSpace - calcSize t) 

dirsThatAreEnough t = 
    let needed = neededDelete t 
    in go needed t where
        go needed t = concatMap (go needed) (subForest t) ++ if calcSize t >= needed && size (rootLabel t) == 0 then [(t, calcSize t)] else []


day7part2 :: IO String -> IO ()
day7part2 input = do
    contents <- input
    print $ neededDelete (commandsToTree (parseInput contents))
    print $ fmap (rootLabel . fst) (dirsThatAreEnough (commandsToTree (parseInput contents)))
    print $ snd (minimumBy (compare `on` snd) (dirsThatAreEnough (commandsToTree (parseInput contents))))

day7part1 :: IO String -> IO ()
day7part1 input = do
    contents <- input
    print $ parseInput contents
    print $ commandsToTree (parseInput contents)
    print $ calcSize (commandsToTree (parseInput contents))
    -- print $ calcSize (subForest (commandsToTree (parseInput contents)) !! 0)
    -- print $ calcSize (subForest (commandsToTree (parseInput contents)) !! 3)
    print $ sumIf (commandsToTree (parseInput contents))

