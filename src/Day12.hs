module Day12 where
import Data.List ( elemIndex, findIndex )
import Data.Set (singleton, union, fromList)


findVertex :: Char -> [String] -> (Int, Int)
findVertex c grid = (x, y) where Just (x, Just y) = fmap (\i -> (i, elemIndex c (grid !! i))) (findIndex (elem c) grid)

dimensions strs = (length strs, length (head strs))

validPos :: (Int, Int) -> (Int, Int) -> Bool
validPos pos dims = fst pos >= 0 && snd pos >= 0 && fst pos < fst dims && snd pos < snd dims

validStep :: [String] -> (Int, Int) -> (Int, Int) -> Bool
validStep grid (xf, yf) (xt, yt) = 
    (validPos (xt, yt) (dimensions grid)) &&
        case ((grid !! xf) !! yf) of
            'S' -> ((grid !! xt) !! yt) == 'a'
            'z' -> True
            c -> (fromEnum (c) + 1 >= fromEnum (((grid !! xt) !! yt))) && ((grid !! xt) !! yt) /= 'E'

neighbours grid (x,y) = filter (validStep grid (x,y)) [(x+1, y), (x-1, y), (x, y+1), (x,y-1)]    

validStep' grid (xt, yt)  (xf, yf) = 
    (validPos (xf, yf) (dimensions grid)) &&
        case ((grid !! xf) !! yf) of
            'S' -> ((grid !! xt) !! yt) == 'a'
            'z' -> True
            c -> (fromEnum (c) + 1 >= fromEnum (((grid !! xt) !! yt))) && ((grid !! xt) !! yt) /= 'E'

neighbours' grid (x,y) = filter (validStep' grid (x,y)) [(x+1, y), (x-1, y), (x, y+1), (x,y-1)]    

bfs :: [String] -> (Int, Int) -> Int
bfs grid s =
    go [(s, 0)] (findVertex 'E' grid) (singleton s) where
        go [] target visited = -1
        go ((node, dist):qs) target visited = 
            if node == target then dist
            else
                go (qs ++ newnodes) target (union visited (fromList (fmap fst newnodes)))
                where newnodes = fmap (\n -> (n, dist+1)) (filter (\n -> not (elem n visited)) (neighbours grid node))

bfs' :: [String] -> Int
bfs' grid =
    go [((findVertex 'E' grid), 0)]  (singleton (findVertex 'E' grid)) where
        go [] visited = -1
        go ((node, dist):qs) visited = 
            if ((grid !! fst node) !! snd node) == 'a' then dist
            else
                go (qs ++ newnodes) (union visited (fromList (fmap fst newnodes)))
                where newnodes = fmap (\n -> (n, dist+1)) (filter (\n -> not (elem n visited)) (neighbours' grid node))

smallElevation :: [String] -> [(Int, Int)]
smallElevation grid = filter (\(x, y) -> ((grid !! x) !! y == 'a')) [(x,y) | x <- [0..fst (dimensions grid)-1], y <- [0..snd (dimensions grid)-1]]    

day12part1 :: IO String -> IO ()
day12part1 input = do 
    contents <- input
    print $ findVertex 'S' (lines contents)
    print $ findVertex 'E' (lines contents)
    print $ neighbours (lines contents) (2, 4)
    print $ validPos (1, 5) (dimensions (lines contents))
    print $ bfs (lines contents) (findVertex 'S' (lines contents))

day12part2 :: IO String -> IO ()
day12part2 input = do
    contents <- input
    print $ bfs' (lines contents)
