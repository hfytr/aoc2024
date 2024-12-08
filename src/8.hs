import Data.List (tails)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

type Coord = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "inputs/8-actual.txt"
  let (width, height, antenna) = parseInput $ lines contents
  print $ length $ solve (p1FreqPairAntinodes (width, height)) antenna
  print $ length $ solve (p2FreqPairAntinodes (width, height)) antenna

solve ::
  ((Coord, Coord) -> [Coord]) ->
  Map Char (Set Coord) ->
  Set Coord
solve freqPairAntinodes antenna =
  foldr
    (\(_, freqCoords) s -> S.union s (foldFreq freqCoords))
    S.empty
    (M.toList antenna)
 where
  foldFreq :: Set Coord -> Set Coord
  foldFreq freqCoords = S.fromList $ concatMap freqPairAntinodes $ setPairs freqCoords

p2FreqPairAntinodes :: Coord -> (Coord, Coord) -> [Coord]
p2FreqPairAntinodes dim ((x1, y1), (x2, y2)) =
  let (dx, dy) = (x2 - x1, y2 - y1)
      antiNodesInDir (xs, ys) dir =
        takeWhile (checkBounds dim) [(dir xs (i * dx), dir ys (i * dy)) | i <- [0 ..]]
  in antiNodesInDir (x1, y1) (-) ++ antiNodesInDir (x1, y1) (+)

p1FreqPairAntinodes :: Coord -> (Coord, Coord) -> [Coord]
p1FreqPairAntinodes dim ((x1, y1), (x2, y2)) =
  let (dx, dy) = (x2 - x1, y2 - y1)
  in [ (x, y) | (x, y) <- [(x1 - dx, y1 - dy), (x2 + dx, y2 + dy)], checkBounds dim (x, y)
     ]

checkBounds :: Coord -> Coord -> Bool
checkBounds (width, height) (x, y) = 0 <= x && x < width && 0 <= y && y < height

setPairs :: Set Coord -> [(Coord, Coord)]
setPairs s = [(x, y) | (x : ytail) <- tails (S.toList s), y <- ytail]

parseInput :: [String] -> (Int, Int, Map Char (Set Coord))
parseInput list =
  ( length list
  , length (head list)
  , foldr
      ( \(row, s) aRow ->
          foldr
            (foldLine row)
            aRow
            (zip [0 ..] s)
      )
      M.empty
      (zip [0 ..] list)
  )
 where
  foldLine ::
    Int -> (Int, Char) -> Map Char (Set Coord) -> Map Char (Set Coord)
  foldLine _ (_, '.') a = a
  foldLine row (col, c) a = M.insertWith S.union c (S.singleton (row, col)) a
