import Data.Array (Array, bounds, listArray, (!))
import Data.List (isPrefixOf, transpose)
import Data.Universe.Helpers (diagonals)

type CharMat = Array Int (Array Int Char)

main :: IO ()
main = do
  contents <- readFile "inputs/4-actual.txt"
  let
    input = lines contents
    n = (length . head) input
    m = listArray (0, length input - 1) [listArray (0, n - 1) line | line <- input]
  putStrLn "Part I:"
  print $ solvep1 $ lines contents
  putStrLn "Part II:"
  print $ solvep2 m

solvep2 :: CharMat -> Int
solvep2 m = sum $ map (xmasOnDiagonal m) $ diagonalStarts m

xmasOnDiagonal :: CharMat -> (Int, Int) -> Int
xmasOnDiagonal m (x, y)
  | x + 2 > (snd . bounds) m || y + 2 > (snd . bounds) (m ! 0) = 0
  | (isPrefixOfDiag m (x, y) "MAS" || isPrefixOfDiag m (x, y) "SAM")
      && isXmas m (x, y) =
      1 + xmasOnDiagonal m (x + 1, y + 1)
  | otherwise = xmasOnDiagonal m (x + 1, y + 1)

isXmas :: CharMat -> (Int, Int) -> Bool
isXmas m (x, y) =
  let below = m ! x ! (y + 2)
      right = m ! (x + 2) ! y
  in (below == 'S' && right == 'M') || (right == 'S' && below == 'M')

isPrefixOfDiag :: CharMat -> (Int, Int) -> String -> Bool
isPrefixOfDiag _ _ "" = True
isPrefixOfDiag m (x, y) (si : stail) = (m ! x ! y) == si && isPrefixOfDiag m (x + 1, y + 1) stail

diagonalStarts :: CharMat -> [(Int, Int)]
diagonalStarts m =
  let width = (snd . bounds) (m ! 0)
      height = (snd . bounds) m
  in [(i, 0) | i <- [0 .. (height - 1)]] ++ [(0, i) | i <- [1 .. (width - 1)]]

solvep1 :: [String] -> Int
solvep1 input =
  let
    solveLine :: String -> Int
    solveLine s'
      | null s' = 0
      | isPrefixOf "XMAS" s' || isPrefixOf "SAMX" s' = 1 + (solveLine . tail) s'
      | otherwise = (solveLine . tail) s'
    solvep1' = sum . map solveLine
  in
    solvep1' input
      + solvep1' (transpose input)
      + (solvep1' . diagonals) input
      + (solvep1' . diagonals . map reverse) input
