import Data.Array (Array, (!))
import Data.Array qualified as Arr
import Data.List (elemIndex)
import Data.Maybe (fromJust, isNothing)
import Data.Sequence (Seq ((:<|), (:|>)), (<|), (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

type Vec2 = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "inputs/18-actual.txt"
  let (corrupted, grid) = parseInput 70 $ lines contents
  putStrLn "Part I:"
  print $ bfs grid 1024
  putStrLn "Part II:"
  print $ solvep2 corrupted grid

solvep2 :: Array Int Vec2 -> Array Vec2 Int -> Vec2
solvep2 corrupted grid = corrupted ! head (filter (isNothing . bfs grid) [0 ..])

bfs :: Array Vec2 Int -> Int -> Maybe Int
bfs grid t = go (Seq.singleton (0, (0, 0))) Set.empty
 where
  go :: Seq (Int, Vec2) -> Set Vec2 -> Maybe Int
  go Seq.Empty _ = Nothing
  go ((dist, cur) :<| toSearch) vis
    | not (checkBounds cur) || Set.member cur vis = go toSearch vis
    | cur == snd (Arr.bounds grid) = Just dist
    | otherwise =
        go
          ( foldr (\p acc -> acc |> (dist + 1, p)) toSearch $
              filter (\p -> checkBounds p && (grid ! p) > t) $
                map (cur +++) dirs
          )
          (Set.insert cur vis)

  checkBounds (x, y) = x >= 0 && x <= maxx && y >= 0 && y <= maxy
  (_, (maxx, maxy)) = Arr.bounds grid

parseInput :: Int -> [String] -> (Array Int Vec2, Array Vec2 Int)
parseInput sideLen list =
  ( Arr.listArray (0, length list) (map parseLine list)
  , Arr.accumArray (\_ x -> x) maxBound ((0, 0), (sideLen, sideLen)) $
      zip (map parseLine list) [0 ..]
  )
 where
  parseLine :: String -> (Int, Int)
  parseLine line =
    let (first, _ : second) = splitAt (fromJust $ elemIndex ',' line) line
     in (read first, read second)

dirs :: [Vec2]
dirs = [(1, 0), (-1, 0), (0, -1), (0, 1)]

(+++) :: Vec2 -> Vec2 -> Vec2
(+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
