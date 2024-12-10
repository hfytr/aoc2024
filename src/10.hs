import Data.Monoid (Sum (Sum, getSum))
import Data.Set qualified as Set
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vec

type Coord = (Int, Int)
type TopoMap = Vector (Vector Int)

main :: IO ()
main = do
  contents <- readFile "inputs/10-actual.txt"
  let input = parseInput $ lines contents
      heads = trailHeads input
  putStrLn "Part I:"
  print $ sum $ map (trailScore input) heads
  putStrLn "Part II:"
  print $ getSum $ sum $ map (trailRating input) heads

trailRating :: TopoMap -> Coord -> Sum Int
trailRating = solve (Sum 0) (const 1)

trailScore :: TopoMap -> Coord -> Int
trailScore topoMap trailHead = Set.size $ solve Set.empty Set.singleton topoMap trailHead

-- trailScore = Set.size .: solve Set.union Set.empty Set.singleton
-- (.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
-- (.:) = (.) . (.)

solve :: (Monoid a) => a -> (Coord -> a) -> TopoMap -> Coord -> a
solve nullVal genSingleton topoMap trailHead = go trailHead (-1)
 where
  go (x, y) from
    | not (checkBounds topoMap (x, y)) = nullVal
    | topoMap ! x ! y /= from + 1 = nullVal
    | topoMap ! x ! y == 9 = genSingleton (x, y)
    | otherwise =
        let curVal = topoMap ! x ! y
        in foldr
            (<>)
            nullVal
            [go (x + dx, y + dy) curVal | (dx, dy) <- [(-1, 0), (0, -1), (1, 0), (0, 1)]]

checkBounds :: TopoMap -> Coord -> Bool
checkBounds topoMap (x, y) =
  x >= 0
    && x < Vec.length topoMap
    && y >= 0
    && y < (Vec.length . Vec.head) topoMap

trailHeads :: TopoMap -> [Coord]
trailHeads topoMap =
  [ (x, y) | x <- [0 .. Vec.length topoMap - 1], y <- [0 .. Vec.length (Vec.head topoMap) - 1], topoMap ! x ! y == 0
  ]

parseInput :: [String] -> TopoMap
parseInput = Vec.fromList . map (Vec.fromList . map (\c -> read [c]))
