main :: IO ()
main = do
  contents <- readFile "inputs/7-actual.txt"
  let input = parseInput $ lines contents
  putStrLn "Part I:"
  print $ solve [(+), (*)] input
  putStrLn "Part II:"
  print $ solve [(+), (*), concatNums] input

solve :: [Int -> Int -> Int] -> [(Int, [Int])] -> Int
solve ops list = sum $ map fst $ filter (\(t, l) -> checkLine ops (head l) t (tail l)) list

checkLine :: [Int -> Int -> Int] -> Int -> Int -> [Int] -> Bool
checkLine _ left target [] = left == target
checkLine ops left target (ni : ntail) = (left < target) && any (\op -> checkLine ops (left `op` ni) target ntail) ops

concatNums :: Int -> Int -> Int
concatNums a b = a * 10 ^ length (show b) + b

parseInput :: [String] -> [(Int, [Int])]
parseInput = map (\s -> ((read . init . head . words) s, map read ((tail . words) s)))
