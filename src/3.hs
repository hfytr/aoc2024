import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = do
  contents <- readFile "inputs/3-actual.txt"
  putStrLn "Part I:"
  print $ solve contents False True "mul("
  putStrLn "Part II:"
  print $ solve contents True True "mul("

solve :: [Char] -> Bool -> Bool -> [Char] -> Int
solve s useEnabled enabled lookingFor
  | null s = 0
  | useEnabled && isPrefixOf "do()" s = solve (drop 4 s) True True "mul("
  | useEnabled && isPrefixOf "don't()" s = solve (drop 7 s) True False "mul("
  | not enabled && useEnabled = solve (tail s) useEnabled False ""
  | null lookingFor = checkMulInstance s useEnabled
  | head s == head lookingFor = solve (tail s) useEnabled True (tail lookingFor)
  | otherwise =
      if lookingFor == "mul("
        then solve (tail s) useEnabled True "mul("
        else solve s useEnabled True "mul("

checkMulInstance :: String -> Bool -> Int
checkMulInstance s useEnabled =
  let (firstNum, afterFirst) = readNum s
      (secondNum, afterSecond) = if firstNum < 0 then (-1, "") else readNum (tail afterFirst)
  in if firstNum < 0 || head afterFirst /= ','
      then solve afterFirst useEnabled True "mul("
      else
        if secondNum < 0 || head afterSecond /= ')'
          then solve afterSecond useEnabled True "mul("
          else firstNum * secondNum + solve afterSecond useEnabled True "mul("

readNum :: String -> (Int, String)
readNum str = if isDigit (head str) then readNum' str 0 else (-1, str)
 where
  readNum' :: String -> Int -> (Int, String)
  readNum' s readSoFar =
    let digit = fromMaybe (-1) (readMaybe [head s])
    in if digit < 0
        then (readSoFar, s)
        else readNum' (tail s) (readSoFar * 10 + digit)
