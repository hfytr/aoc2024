import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)

main :: IO ()
main = do
  contents <- readFile "inputs/19-small.txt"
  let input = parseInput $ lines contents
  putStrLn "Part I:"
  print $ solvep1 input
  putStrLn "Part II:"
  print $ solvep2 input

solvep1 :: ([String], [String]) -> Int
solvep1 (towels, patterns) = length $ filter ((> 0) . numWays towels) patterns

solvep2 :: ([String], [String]) -> Int
solvep2 (towels, patterns) = foldr ((+) . numWays towels) 0 patterns

numWays :: [String] -> String -> Int
numWays towels startPattern = fst $ go Map.empty startPattern
 where
  go :: Map String Int -> String -> (Int, Map String Int)
  go _ "" = (1, Map.empty)
  go vis pattern
    | Map.member pattern vis = (fromJust $ Map.lookup pattern vis, Map.empty)
    | otherwise =
        let (res, resVis) =
              foldr
                ( \t (acc, accVis) ->
                    let (child, childVis) = maybe (0, Map.empty) (go accVis) (stripPrefix t pattern)
                     in (acc + child, Map.union childVis accVis)
                )
                (0, vis)
                towels
         in (res, Map.insert pattern res resVis)

parseInput :: [String] -> ([String], [String])
parseInput (towels : "" : patterns) = (splitOn ", " towels, patterns)
parseInput _ = error "Invalid input."
