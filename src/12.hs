import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace

type Vec2 = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "inputs/12-actual.txt"
  let regions =
        concatMap (connectedComponents . snd) $ Map.toList $ getTypes $ lines contents
  putStrLn "Part I:"
  print $ solve borders regions
  putStrLn "Part II:"
  print $ solve sides regions

solve :: (Set Vec2 -> Int) -> [Set Vec2] -> Int
solve op =
  foldr
    (\r acc -> (Set.size r * op r) + acc)
    0

sides :: Set Vec2 -> Int
sides pts =
  let dirPairs = zip dirs $ tail $ cycle dirs
   in foldr
        ( \p acc ->
            acc
              + length
                [ 0
                | (d1, d2) <- dirPairs
                , Set.member (p +++ d1) pts == Set.member (p +++ d2) pts
                    && (Set.notMember (p +++ d1 +++ d2) pts || Set.notMember (p +++ d2) pts)
                ]
        )
        0
        pts

borders :: Set Vec2 -> Int
borders pts =
  length $
    filter (`Set.notMember` pts) $
      Set.foldr addAdj [] pts
  where
    addAdj c = (++) [c +++ d | d <- dirs]

getTypes :: [String] -> Map Char (Set Vec2)
getTypes list =
  foldr
    ( \(i, s) acc ->
        foldr
          (\(j, c) acc' -> Map.insertWith Set.union c (Set.singleton (i, j)) acc')
          acc
          (zip [0 ..] s)
    )
    Map.empty
    (zip [0 ..] list)

connectedComponents :: Set Vec2 -> [Set Vec2]
connectedComponents pts
  | null pts = []
  | otherwise =
      let component = head (Set.toList pts) `getComponent` Set.empty
       in component : connectedComponents (Set.filter (`Set.notMember` component) pts)
  where
    getComponent :: Vec2 -> Set Vec2 -> Set Vec2
    getComponent c vis
      | Set.member c vis || Set.notMember c pts = Set.empty
      | otherwise =
          foldr
            (\c' acc -> Set.union acc $ getComponent c' acc)
            (Set.insert c vis)
            [c +++ d | d <- dirs]

dirs :: [Vec2]
dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

(+++) :: Vec2 -> Vec2 -> Vec2
(+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
