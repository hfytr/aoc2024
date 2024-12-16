import Control.Monad (liftM2)
import Data.List (stripPrefix)
import Data.Maybe (fromJust)

type LinEq = ((Int, Int), (Int, Int), (Int, Int))

main :: IO ()
main = do
  contents <- readFile "inputs/13-actual.txt"
  let input = parseInput $ lines contents
  putStrLn "Part I:"
  print $ solve input 0
  putStrLn "Part II:"
  print $ solve input 10000000000000

solve :: [LinEq] -> Int -> Int
solve input offset =
  sum $ map (maybe 0 (\(a, b) -> 3 * a + b) . solveLinEq . offsetB offset) input

solveLinEq :: LinEq -> Maybe (Int, Int)
solveLinEq ((a11, a21), (a12, a22), (b1, b2)) =
  let det = a11 * a22 - a12 * a21
      x1 = maybeDiv (b1 * a22 - b2 * a12) det
      x2 = maybeDiv (a11 * b2 - a21 * b1) det
   in liftM2 (,) x1 x2

maybeDiv :: Int -> Int -> Maybe Int
maybeDiv a b = if rem a b == 0 then Just (div a b) else Nothing

offsetB :: Int -> LinEq -> LinEq
offsetB offset (a1, a2, (b1, b2)) = (a1, a2, (b1 + offset, b2 + offset))

parseInput :: [String] -> [LinEq]
parseInput (line1 : line2 : line3 : _ : rest) =
  let parseLine :: String -> String -> (Int, Int)
      parseLine s prefix =
        let (w1 : w2 : _) = words $ fromJust $ stripPrefix prefix s
         in (read (init w1), (read . tail . tail) w2)
   in ( parseLine line1 "Button A: X+"
      , parseLine line2 "Button B: X+"
      , parseLine line3 "Prize: X="
      )
        : parseInput rest
parseInput _ = []
