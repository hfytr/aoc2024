import Data.Ix

main :: IO ()
main = do
  contents <- readFile "inputs/2-actual.txt"

  let
    parsedLines = map (map (read :: String -> Int) . words) $ lines contents

    part1Result =
      length $
        filter
          isSafe
          parsedLines
    part2Result =
      length $
        filter
          isListFixable
          parsedLines

  putStrLn "Part I:"
  print part1Result
  putStrLn "Part II:"
  print part2Result

isListFixable :: [Int] -> Bool
isListFixable list = any isSafe $ oneRemoved list

oneRemoved :: [Int] -> [[Int]]
oneRemoved [] = []
oneRemoved (l1 : ltail) = ltail : map (l1 :) (oneRemoved ltail)

isSafe :: [Int] -> Bool
isSafe list =
  let diffs = zipWith (-) list (tail list)
  in sameSign diffs && all (inRange (1, 3) . abs) diffs

sameSign :: [Int] -> Bool
sameSign list
  | head list > 0 = all (> 0) list
  | head list < 0 = all (< 0) list
  | otherwise = False

-- isListFixable :: [Int] -> Bool
-- isListFixable list =
-- let
--   diffsToCheck = take 3 (zipWith (-) list (tail list))
--   checkDiff a b =
--     inRange (1, 3) (abs (a - b))
--       && (if length (filter (> 0) diffsToCheck) > 1 then a > b else a < b)
--   unitOffset = fromEnum $ checkDiff 0 1
--   toPrepend = [(list !! 1) - unitOffset | not (checkDiff (head list) (list !! 1))]
--   (penultimate, ultimate) = lastTwo list
--   toAppend = [penultimate + unitOffset | not (checkDiff penultimate ultimate)]
--   -- checks whether we remove max one of the middle elements
--   isListFixable' (l1 : l2 : l3 : ltail) =
--     checkDiff l1 l2
--       && if checkDiff l2 l3
--         then isListFixable' (l2 : l3 : ltail)
--         else if checkDiff l1 l3 then isListFixable' (l1 : l3 : ltail)
--         else
--   isListFixable' _ = True
-- in
--   isListFixable' $ toPrepend ++ list ++ toAppend

-- lastTwo :: [Int] -> (Int, Int)
-- lastTwo (l1 : l2 : []) = (l1, l2)
-- lastTwo (_ : ltail) = lastTwo ltail
