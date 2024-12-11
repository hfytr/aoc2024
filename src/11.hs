import Data.Map (Map)
import Data.Map qualified as Map

main :: IO ()
main = do
  contents <- readFile "inputs/11-actual.txt"
  let nums :: [Int] = map read $ words contents
  print nums
  print $ solvep1 75 nums

solvep1 :: Int -> [Int] -> Int
solvep1 n nums =
  Map.foldr (+) 0 $
    iterate blink (Map.fromListWith (+) $ map (,1) nums) !! n

blink :: Map Int Int -> Map Int Int
blink = go . Map.toList
 where
  go :: [(Int, Int)] -> Map Int Int
  go [] = Map.empty
  go ((0, freq) : xtail) = Map.unionWith (+) (go xtail) $ Map.singleton 1 freq
  go ((num, freq) : xtail) = case cutNum num of
    Just (left, right) ->
      Map.unionWith (+) (go xtail) $
        Map.insertWith (+) left freq $
          Map.singleton right freq
    Nothing -> Map.unionWith (+) (go xtail) $ Map.singleton (2024 * num) freq

cutNum :: Int -> Maybe (Int, Int)
cutNum x =
  let digits = length (show x)
  in if even digits
      then
        let (left, right) = splitAt (div digits 2) (show x)
        in Just (read left, read right)
      else Nothing
