import qualified Data.HashMap.Strict as HashMap
import Data.List (sort)
import Data.Monoid
import System.IO

main :: IO ()
main = do
  handle <- openFile "inputs/1-actual.txt" ReadMode
  contents <- hGetContents handle

  putStrLn "Part I:"
  print $ sum $ zipWith (\a b -> abs (a - b)) (sort (column 0 contents)) (sort (column 1 contents))

  putStrLn "Part II:"
  print $ getSum $ foldHashMap $ buildHashMap HashMap.empty (column 0 contents) (column 1 contents) 0

  hClose handle

foldHashMap :: HashMap.HashMap Int Int -> Sum Int
foldHashMap = HashMap.foldMapWithKey (\x y -> Sum (x * y))

buildHashMap :: HashMap.HashMap Int Int -> [Int] -> [Int] -> Int -> HashMap.HashMap Int Int
buildHashMap hashmap column1 column2 i
  | HashMap.null hashmap = buildHashMap (initHashMap hashmap column1 0) column1 column2 i
  | i >= length column2 = hashmap
  | otherwise = buildHashMap (HashMap.adjust (+ 1) (column2 !! i) hashmap) column1 column2 (i + 1)

initHashMap :: HashMap.HashMap Int Int -> [Int] -> Int -> HashMap.HashMap Int Int
initHashMap hashmap list i
  | i >= length list = hashmap
  | otherwise = initHashMap (HashMap.insert (list !! i) 0 hashmap) list (i + 1)

column :: Int -> String -> [Int]
column col contents = map (\line -> read (words line !! col)) (lines contents)
