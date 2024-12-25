import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  contents <- readFile "inputs/25-small.txt"
  let (keys, locks, height) = parseInput $ lines contents
  print (keys, locks, height)
  print $ solvep1 keys locks height

solvep1 :: [[Int]] -> [[Int]] -> Int -> Int
solvep1 keys locks height =
  length
    [ (key, lock)
    | key <- keys
    , lock <- locks
    , all (<= height) $ zipWith (+) lock key
    ]

parseInput :: [String] -> ([[Int]], [[Int]], Int)
parseInput list =
  let (keys, locks) =
        foldr
          ( \l (accKeys, accLocks) ->
              case parseBlock l of
                Right key -> (key : accKeys, accLocks)
                Left lock -> (accKeys, lock : accLocks)
          )
          ([], [])
          $ splitOn [""] list
   in (keys, locks, fromJust (elemIndex "" list) - 2)
  where
    parseBlock :: [String] -> Either [Int] [Int]
    parseBlock block =
      let heights = foldr (zipWith (\c h -> if c == '#' then h + 1 else h)) [0 | _ <- tail $ init block] $ tail $ init block
       in case head (head block) of
            '#' -> Left heights
            '.' -> Right heights
            _ -> error "Invalid char in block."
