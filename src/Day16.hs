module Day16 where
import Data.Map (Map, fromList, (!), assocs, keys)
import Text.Trifecta
    ( letter,
      sepBy,
      integer,
      parseString,
      some,
      CharParsing(string),
      Parser,
      Result(Success), try )

import Data.Maybe (mapMaybe)
import Data.Set (Set, fromList, member, singleton, union)
import Data.List (singleton, sort, sortBy)
import Data.PQueue.Max (MaxQueue, singleton, fromList, deleteFindMax, toList, union)
import Control.Applicative ((<|>))
import Debug.Trace (trace)
import Data.Function (on)

data Tunnels = Tunnels {
    rates :: Map String Integer,
    neighbours :: Map String [String] 
    } deriving (Show, Eq)

data Step = Move String | Open String deriving (Show, Eq, Ord)

data PartialSolution = PSol {
    maxFlow :: Integer,
    currentFutureFlow :: Integer,
    steps :: [Step]
    } deriving (Show, Eq, Ord)

bfs :: Tunnels -> String -> String -> Int
bfs tunnels s e =
    go (Data.List.singleton (s, 0)) e (Data.Set.singleton s)where
        go [] target visited = -1
        go ((node, dist):qs) target visited = 
            if node == target then dist
            else
                go (qs ++ newnodes) target (Data.Set.union visited (Data.Set.fromList (fmap fst newnodes)))
                where newnodes = fmap (\n -> (n, dist+1)) (filter (\n -> not (elem n visited)) (neighbours tunnels ! node))

precomputebfs :: Tunnels -> Map String Int
precomputebfs tunnels = Data.Map.fromList([(node ++ node2, bfs tunnels node node2) | node <- keys (rates tunnels), node2 <- keys (rates tunnels)])

location :: PartialSolution -> String
location psol = 
    go (steps psol) where
        go [] = "AA"
        go (Open s:sts) = s
        go (Move s:sts) = s

openValves :: [Step] -> Set String
openValves sts = Data.Set.fromList (mapMaybe (\s ->
    case s of
        Open s' -> Just s'
        Move _ -> Nothing) sts)

-- extra flow obtainable
heuristic :: Tunnels -> [Step] -> Integer
heuristic tunnels sts = (30 - (min 30 (fromIntegral (length sts))) - 1)*sum (mapMaybe (\(l, i) -> if member l (openValves sts) then Nothing else Just i) (assocs (rates tunnels)))

tunnel :: Parser (String, Integer, [String])
tunnel = do
    string "Valve "
    id <- some letter
    string " has flow rate="
    i <- integer
    try (string "; tunnels lead to valves ") <|> string "; tunnel leads to valve "
    targets <- some letter `sepBy` string ", "
    return (id, i, targets)

optimisticTunnel :: String -> (String, Integer, [String])
optimisticTunnel s = r where (Success r) = parseString tunnel mempty s

parseTunnels :: String -> Tunnels
parseTunnels contents = 
    let tunnels = fmap optimisticTunnel (lines contents)
    in Tunnels (Data.Map.fromList (fmap (\(id, rate, _) -> (id, rate)) tunnels)) (Data.Map.fromList (fmap (\(id, _, neighbours) -> (id, neighbours)) tunnels))

transformTargetNodeToSteps :: Map String Int -> Tunnels -> String -> (String, Integer) -> [Step]
transformTargetNodeToSteps bfsMap tunnels start (target, rate) = Open target:replicate (bfsMap ! (start ++ target)) (Move target)


getSteps :: Map String Int ->Tunnels -> String -> [Step] -> [[Step]]
getSteps bfsMap tunnels s s' = sortBy (compare `on` length) (fmap (transformTargetNodeToSteps bfsMap tunnels s) ( ((filter (\(node, rate) -> node /= s && rate > 0 && not (member node (openValves s')))  (assocs (rates tunnels))))))

validSteps :: Map String Int -> Tunnels -> [Step] -> [[Step]]
validSteps bfsMap tunnels [] = [Open "AA"]:getSteps bfsMap tunnels "AA" []
validSteps bfsMap tunnels s'@(Open s:sts) = getSteps bfsMap tunnels s s'
validSteps bfsMap tunnels s'@(Move s:sts) = if member s (openValves sts) || (rates tunnels ! s == 0)
                                            then getSteps bfsMap tunnels s s'
                                            else [Open s]:getSteps bfsMap tunnels s s'

computeNewSols :: Map String Int -> Tunnels -> PartialSolution -> MaxQueue PartialSolution
computeNewSols bfsMap tunnels psol = Data.PQueue.Max.fromList (fmap f (validSteps bfsMap tunnels (steps psol))) where
    f :: [Step] -> PartialSolution
    -- f psol [] = trace ("Psol = " ++ show psol) (PSol 0 0 [])
    f ((Open s):xs) = PSol (updatedFlow + heuristic tunnels (Open s:xs ++ steps psol)) updatedFlow (Open s:xs ++ steps psol) where
                    updatedFlow = (currentFutureFlow psol) + (rates tunnels ! s) * (30 - (min 30 (fromIntegral (length (steps psol) + length xs))) -1)
    f ((Move s):xs) = PSol ((currentFutureFlow psol) + heuristic tunnels (Move s:xs ++ steps psol)) (currentFutureFlow psol) (Move s:xs ++  steps psol)

solve :: Tunnels -> Integer
solve tunnels =
    go tunnels 0 (Data.PQueue.Max.singleton (PSol (heuristic tunnels []) 0 [])) where
        go :: Tunnels -> Integer -> MaxQueue PartialSolution -> Integer
        go tunnels bestSol q = 
            let (curSol, sols) = trace (show bestSol) (deleteFindMax q)
            in if trace (show (maxFlow curSol)) maxFlow curSol <= bestSol then bestSol
               else 
                if length (steps curSol) >= 30 then go tunnels bestSol sols
                else go tunnels (maximum (bestSol:(fmap currentFutureFlow (toList (newSols curSol))))) (Data.PQueue.Max.union sols (newSols curSol))
                        where newSols curSol = computeNewSols (precomputebfs tunnels) tunnels curSol
                                    

day16part1 :: IO String -> IO ()
day16part1 input = do
    contents <- input
    print $ parseString tunnel mempty "Valve JJ has flow rate=21; tunnel leads to valve II"
    print $ solve (parseTunnels contents)