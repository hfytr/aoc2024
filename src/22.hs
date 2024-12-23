import Data.Bits (Bits (xor))
import Data.Map qualified as Map
import Data.Vector qualified as Vec

main :: IO ()
main = do
  contents <- readFile "inputs/22-actual.txt"
  let input = map read $ lines contents
  putStrLn "Part I:"
  print $ solvep1 input
  putStrLn "Part II:"
  print $ solvep2 input

solvep2 :: [Int] -> Int
solvep2 nums = Map.foldr max (-1) diffBananas
 where
  diffBananas =
    foldr
      ( \(monkeDiff, monkePrice) acc ->
          Map.unionWith (+) acc $
            foldr
              ( \(i, price) acc' ->
                  Map.insertWith
                    const
                    (Vec.slice i 4 monkeDiff)
                    price
                    acc'
              )
              Map.empty
              (zip [0 ..] (drop 4 monkePrice))
      )
      Map.empty
      (zip diffs prices)
  diffs = map (\l -> Vec.fromList $ zipWith (-) (tail l) l) prices
  prices = map (map (`mod` 10)) $ secrets nums

solvep1 :: [Int] -> Int
solvep1 nums = sum $ map last $ secrets nums

secrets :: [Int] -> [[Int]]
secrets = map (take 2001 . iterate evolveNum)
 where
  evolveNum :: Int -> Int
  evolveNum x =
    let first = ((x * 64) `xor` x) `mod` 16777216
        second = (div first 32 `xor` first) `mod` 16777216
     in ((second * 2048) `xor` second) `mod` 16777216
