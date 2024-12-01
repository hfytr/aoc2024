import Data.List (sort)
import GHC.Base (eqInt)
import System.IO

main :: IO ()
main = do
  handle <- openFile "inputs/1-actual.txt" ReadMode
  contents <- hGetContents handle

  putStrLn "Part I:"
  print $ sum $ zipWith diff (column 0 (lines contents)) (column 1 (lines contents))

  putStrLn "Part II:"
  -- slow O(n^2) way
  print $ sum $ map (entrySimScore (column 1 (lines contents))) (column 0 (lines contents))

  hClose handle

entrySimScore :: [Int] -> Int -> Int
entrySimScore list a = a * length (filter (eqInt a) list)

diff :: Int -> Int -> Int
diff a b = abs $ a - b

column :: Int -> [String] -> [Int]
column col lines_ = sort (map (parseLine col) lines_)

parseLine :: Int -> String -> Int
parseLine col line = read (words line !! col)
