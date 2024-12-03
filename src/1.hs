import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid
import System.IO

main :: IO ()
main = do
  handle <- openFile "inputs/1-actual.txt" ReadMode
  contents <- hGetContents handle

  putStrLn "Part I:"
  print $
    sum $
      zipWith
        (\a b -> abs (a - b))
        (sort (column 0 contents))
        (sort (column 1 contents))

  putStrLn "Part II:"
  print $
    getSum $
      Map.foldMapWithKey
        (\x y -> Sum (x * y))
        ( foldr
            (Map.adjust (+ 1))
            (initMap Map.empty (column 0 contents) 0)
            (column 1 contents)
        )

  hClose handle

initMap ::
  Map Int Int -> [Int] -> Int -> Map Int Int
initMap hashmap list i
  | i >= length list = hashmap
  | otherwise = initMap (Map.insert (list !! i) 0 hashmap) list (i + 1)

column :: Int -> String -> [Int]
column col contents = map (\line -> read (words line !! col)) (lines contents)
