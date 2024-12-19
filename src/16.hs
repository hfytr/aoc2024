import Data.Array (Array, (!))
import Data.Array qualified as Arr
import Data.Heap qualified as Heap
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

type Vec2 = (Int, Int)
data Direction = U | R | D | L deriving (Enum, Show, Ord, Eq)
type FrontierHeap = Heap.MinPrioHeap Int ((Vec2, Direction), Vec2)
type VisMap = Map (Vec2, Direction) (Int, Set Vec2)

main :: IO ()
main = do
  contents <- readFile "inputs/16-actual.txt"
  let grid = parseInput $ lines contents
      result = solve grid
      (_, part1, _) = getBounds result grid
  putStrLn "Part I:"
  print part1
  putStrLn "Part II:"
  print $ Set.size $ retrace result grid

solve :: Array (Int, Int) Bool -> VisMap
solve grid = go Map.empty $ Heap.singleton (0, ((start, R), (-1, -1)))
 where
  end = (1, (snd . snd . Arr.bounds) grid - 1)
  start = ((fst . snd . Arr.bounds) grid - 1, 1)
  go :: VisMap -> FrontierHeap -> VisMap
  go vis oldFrontier =
    let ((dist, (state@(coord, dir), parentCoord)), poppedFrontier) = fromJust (Heap.view oldFrontier)
        frontier =
          foldr
            Heap.insert
            poppedFrontier
            ( [ (dist + 1000, ((coord, clockwise dir), parentCoord))
              , (dist + 1000, ((coord, antiClockwise dir), parentCoord))
              ]
                ++ [ (dist + 1, ((coord +++ asVec2 dir, dir), coord)) | grid ! (coord +++ asVec2 dir)
                   ]
            )
     in case (Map.lookup state vis, coord == end) of
          (Nothing, True) -> Map.insert state (dist, Set.singleton parentCoord) vis
          (Nothing, False) -> go (Map.insert state (dist, Set.singleton parentCoord) vis) frontier
          (Just (oldDist, oldParents), _) -> case compare oldDist dist of
            EQ -> go (Map.insert state (dist, Set.insert parentCoord oldParents) vis) frontier
            LT -> go vis poppedFrontier
            GT -> go (Map.insert state (dist, Set.singleton parentCoord) vis) frontier

retrace :: VisMap -> Array (Int, Int) Bool -> Set Vec2
retrace vis grid = go [endState]
 where
  addParentDirs pos = map (\p -> (p, fromVec2 (pos -++ p)))
  (startCoord, _, endState) = getBounds vis grid
  go :: [(Vec2, Direction)] -> Set Vec2
  go [] = error "Reached empty frontier before finishing."
  go (fi@(pos, _) : ftail)
    | pos == startCoord = Set.singleton startCoord
    | otherwise =
        Set.insert pos $
          go $
            ftail
              ++ (addParentDirs pos . Set.toList . snd . fromJust . Map.lookup fi) vis

getBounds :: VisMap -> Array (Int, Int) Bool -> (Vec2, Int, (Vec2, Direction))
getBounds vis grid =
  let end = (1, (snd . snd . Arr.bounds) grid - 1)
      start = ((fst . snd . Arr.bounds) grid - 1, 1)
      (endState, (dist, _)) = head $ mapMaybe (\d -> ((end, d),) <$> Map.lookup (end, d) vis) [L, R, D, U]
   in (start, dist, endState)

parseInput :: [String] -> Array Vec2 Bool
parseInput list =
  Arr.array
    ((0, 0), (length list - 1, length (head list) - 1))
    $ foldr
      ( \(i, s) acc ->
          foldr
            ( \(j, c) acc' -> case c of
                '#' -> ((i, j), False) : acc'
                _ -> ((i, j), True) : acc'
            )
            acc
            (zip [0 ..] s)
      )
      []
      (zip [0 ..] list)

infixl 6 +++
(+++) :: Vec2 -> Vec2 -> Vec2
(+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

infixl 6 -++
(-++) :: Vec2 -> Vec2 -> Vec2
(-++) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

fromVec2 :: Vec2 -> Direction
fromVec2 (-1, 0) = U
fromVec2 (1, 0) = D
fromVec2 (0, -1) = L
fromVec2 (0, 1) = R
fromVec2 _ = error "Invalid vector passed to fromVec2."

asVec2 :: Direction -> Vec2
asVec2 U = (-1, 0)
asVec2 D = (1, 0)
asVec2 L = (0, -1)
asVec2 R = (0, 1)

clockwise :: Direction -> Direction
clockwise d = toEnum $ (fromEnum d + 1) `mod` 4

antiClockwise :: Direction -> Direction
antiClockwise d = toEnum $ (fromEnum d - 1) `mod` 4
