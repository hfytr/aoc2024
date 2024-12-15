import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)

type Vec2 = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "inputs/12-small.txt"
  let regions =
        concatMap (connectedComponents . snd) $ Map.toList $ getTypes $ lines contents
  putStrLn "Part I:"
  putStrLn $
    foldr
      ( \r s ->
          s
            ++ ('\n' : '\n' : show r ++ "   " ++ show (length r))
            ++ "\n"
            ++ show (borders r)
      )
      ""
      regions
  print $ solvep1 regions
  putStrLn "Part II:"

solvep1 :: [Set Vec2] -> Int
solvep1 =
  foldr
    (\r acc -> (traceShowId (Set.size r) * traceShowId (length (borders r))) + acc)
    0

borders :: Set Vec2 -> [[Vec2]]
borders pts =
  connectedComponents $
    filter (`Set.notMember` pts) $
      Set.foldr addAdj [] pts
 where
  addAdj c = (++) [c +++ d | d <- [(0, 1), (1, 0), (-1, 0), (0, -1)]]

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
      let component = head (Set.toList pts) `getComponent` pts
       in component : connectedComponents (Set.filter (`Set.notMember` component) pts)
 where
  getComponent :: Vec2 -> Set Vec2 -> Set Vec2
  getComponent c pts'
    | Set.notMember c pts' = Set.empty
    | otherwise =
        foldr
          (\c' acc -> Set.union acc $ getComponent c' $ Set.delete c pts')
          (Set.singleton c)
          [c +++ d | d <- [(0, 1), (1, 0), (-1, 0), (0, -1)]]

(+++) :: Vec2 -> Vec2 -> Vec2
(+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
