import Control.Arrow ((***))
import Data.List (sortBy)
import Data.Map (Map, insertWith)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set, union)
import Data.Set qualified as Set

type TupleSet = Map Int (Set Int)

main :: IO ()
main = do
  contents <- readFile "inputs/5-actual.txt"
  let (first, second) = splitAtFunc null $ lines contents
      rules = buildRulesMap first
      pages :: [[Int]]
      pages = map (map read . splitBy ',') second

  putStrLn "Part I:"
  print $ solvep1 (fst rules) pages
  putStrLn "Part II:"
  print $ solvep2 rules pages

solvep2 :: (TupleSet, TupleSet) -> [[Int]] -> Int
solvep2 rules pages =
  sum $
    map (middleElem . fixPage rules) $
      filter (not . checkPage (fst rules)) pages

fixPage :: (TupleSet, TupleSet) -> [Int] -> [Int]
fixPage (rules, invertedRules) = sortBy cmpElements
 where
  cmpElements :: Int -> Int -> Ordering
  cmpElements a b
    | (null . Map.lookup b) rules && (null . Map.lookup a) rules = EQ
    | tupleMember rules (a, b) = LT
    | tupleMember invertedRules (a, b) = GT
    | any
        (\c -> cmpElements c b == LT)
        ((Set.toList . fromJust . Map.lookup a) rules) =
        LT
    | any
        (\c -> cmpElements c b == GT)
        ((Set.toList . fromJust . Map.lookup a) invertedRules) =
        GT
    | otherwise = EQ

solvep1 :: TupleSet -> [[Int]] -> Int
solvep1 rules pages = sum $ map middleElem $ filter (checkPage rules) pages

middleElem :: [a] -> a
middleElem list = list !! div (length list) 2

checkPage :: TupleSet -> [Int] -> Bool
checkPage _ [] = True
checkPage rules (phead : ptail) = checkSuffix phead ptail && checkPage rules ptail
 where
  checkSuffix :: Int -> [Int] -> Bool
  checkSuffix _ [] = True
  checkSuffix q (li : ltail) = notTupleMember rules (li, q) && checkSuffix q ltail

splitBy :: Char -> String -> [String]
splitBy c s
  | null s = [""]
  | head s == c = "" : splitBy c (tail s)
  | otherwise =
      let result = splitBy c $ tail s
      in (head s : head result) : tail result

buildRulesMap :: [String] -> (TupleSet, TupleSet)
buildRulesMap = foldr parseLineKey' (Map.empty, Map.empty)
 where
  parseLineKey' :: String -> (TupleSet, TupleSet) -> (TupleSet, TupleSet)
  parseLineKey' line (result0, result1) =
    let (first, second) = (read *** read) $ splitAtFunc ('|' ==) line
    in ( addNullEntry second (insertTuple result0 (first, second))
       , addNullEntry first (insertTuple result1 (second, first))
       )

addNullEntry :: Int -> TupleSet -> TupleSet
addNullEntry a = insertWith union a Set.empty

insertTuple :: TupleSet -> (Int, Int) -> TupleSet
insertTuple tupleSet (first, second) = insertWith union first (Set.singleton second) tupleSet

tupleMember :: TupleSet -> (Int, Int) -> Bool
tupleMember rulesMap (first, second) = second `Set.member` fromJust (Map.lookup first rulesMap)

notTupleMember :: TupleSet -> (Int, Int) -> Bool
notTupleMember rulesMap tuple = not $ tupleMember rulesMap tuple

-- doesn't include element which gets split on
splitAtFunc :: (a -> Bool) -> [a] -> ([a], [a])
splitAtFunc _ [] = ([], [])
splitAtFunc f (x : xtail)
  | f x = ([], xtail)
  | otherwise =
      let (prefix, suffix) = splitAtFunc f xtail
      in (x : prefix, suffix)
