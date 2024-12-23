{-# LANGUAGE LambdaCase #-}

type Vec2 = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "inputs/21-small.txt"
  let input = parseInput $ lines contents
  print $ solvep1 input

solvep1 :: ([[Vec2]], [Int]) -> Int
solvep1 (allCoords, nums) = sum $ zipWith (\a b -> length (iterate getPresses a !! 3) * b) allCoords nums
 where
  getPresses :: Vec2 -> [Vec2] -> [Vec2]
  getPresses _ [] = []
  getPresses p (ci : ctail) = undefined

parseInput :: [String] -> ([[Vec2]], [Int])
parseInput list =
  ( map
      ( map
          ( \case
              'A' -> (3, 2)
              '0' -> (3, 1)
              c -> let d = read [c] - 1 in (2 - div d 3, d `mod` 3)
          )
      )
      list
  , map (read . init) list
  )
