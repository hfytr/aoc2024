import Data.List (elemIndex, intercalate)
import Data.Maybe (fromJust)
import Data.Set qualified as Set

type Vec2 = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "inputs/14-actual.txt"
  let input = parseInput $ lines contents
      toPrint :: [String]
      toPrint = take 10000 $ map (\i -> showBoard (101, 103) i input) [0 ..]
  putStrLn "Part I:"
  print $ solve (101, 103) 100 input
  mapM_ putStr toPrint

showBoard :: Vec2 -> Int -> [(Vec2, Vec2)] -> String
showBoard (xbound, ybound) time robos =
  let pts = Set.fromList $ map (\(p, v) -> (p +++ v *** time) %%% (xbound, ybound)) robos
   in show time
        ++ "\n"
        ++ intercalate
          "\n"
          ( map
              ( \i ->
                  foldr
                    ( \j acc' -> (if Set.member (j, i) pts then '&' : acc' else '.' : acc')
                    )
                    ""
                    [0 .. (xbound - 1)]
              )
              [0 .. (ybound - 1)]
          )

solve :: Vec2 -> Int -> [(Vec2, Vec2)] -> Int
solve (xbound, ybound) time robots =
  let
    quadrant :: Vec2 -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
    quadrant (x, y) (q1, q2, q3, q4)
      | x < div xbound 2 && y < div ybound 2 = (q1 + 1, q2, q3, q4)
      | x > div xbound 2 && y < div ybound 2 = (q1, q2 + 1, q3, q4)
      | x < div xbound 2 && y > div ybound 2 = (q1, q2, q3 + 1, q4)
      | x > div xbound 2 && y > div ybound 2 = (q1, q2, q3, q4 + 1)
      | otherwise = (q1, q2, q3, q4)
    (q1, q2, q3, q4) =
      foldr
        (\(p, v) acc -> quadrant ((p +++ v *** time) %%% (xbound, ybound)) acc)
        (0, 0, 0, 0)
        robots
   in
    q1 * q2 * q3 * q4

parseInput :: [String] -> [(Vec2, Vec2)]
parseInput =
  foldr
    ( \s acc ->
        let (coordWord : velWord : _) = words s
         in (parseWord coordWord, parseWord velWord) : acc
    )
    []

parseWord :: String -> Vec2
parseWord (_ : _ : s) =
  let (xraw, ',' : yraw) = splitAt (fromJust (elemIndex ',' s)) s
   in (read xraw, read yraw)

infixl 7 %%%
(%%%) :: Vec2 -> Vec2 -> Vec2
(%%%) (x, y) (m, n) = (x `mod` m, y `mod` n)

infixl 7 ***
(***) :: Vec2 -> Int -> Vec2
(***) (x, y) c = (x * c, y * c)

infixl 6 +++
(+++) :: Vec2 -> Vec2 -> Vec2
(+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
