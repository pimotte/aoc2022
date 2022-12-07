module Day7 where
import Text.Trifecta (parseString, string, char, noneOf, many, notChar, try, integer, manyTill)
import Text.Trifecta.Result
import Text.Trifecta.Parser
import Text.Trifecta.Combinators
import Control.Applicative
import Data.Tree

data File = File {
    name :: String,
    size :: Integer
} deriving (Show, Eq)

data Command = Cd String | Ls [File] deriving (Show, Eq)

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
replaceInTree [] files tree = Node (rootLabel tree) (fmap newSubtree files) where
    newSubtree file = Node file []
replaceInTree [dir] files tree = Node (rootLabel tree) (if dir == name (rootLabel tree) then fmap newSubtree files else []) where
    newSubtree file = Node file []
replaceInTree (dir:dirs) files tree = 
    Node (rootLabel tree) (fmap maybeTransform (subForest tree)) where
        maybeTransform subTree = replaceInTree dirs files subTree 

                                            

commandsToTree :: [Command] -> Tree File
commandsToTree commands = go ["/"] commands (Node (File "/" 0) []) where
                                go _ [] tree = tree
                                go curDir ((Cd arg):cs) tree = 
                                    case arg of
                                        ".." -> go (tail curDir) cs tree
                                        "/" -> go ["/"] cs tree
                                        s -> go (s:curDir) cs tree
                                go curDir ((Ls files):cs) tree = go curDir cs (replaceInTree (reverse curDir) files tree)
            



day7part1 :: IO String -> IO ()
day7part1 input = do
    contents <- input
    print $ parseInput contents
    print $ commandsToTree (parseInput contents)

