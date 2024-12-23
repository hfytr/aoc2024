import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set

type Computer = (Char, Char)

main :: IO ()
main = do
  contents <- readFile "inputs/23-actual.txt"
  let input = parseInput $ lines contents
  putStrLn "Part I:"
  print $ solvep1 input
  putStrLn "Part II:"
  putStrLn $ init $ foldr (\(c1, c2) acc -> c1 : c2 : ',' : acc) "" $ Set.toAscList $ biggestClique input

solvep1 :: Map Computer (Set Computer) -> Int
solvep1 adj = div (length $ filter startsWithT $ stronkTriples adj) 6 -- divide by 6 as we get every permutation of triples
  where
    startsWithT ((c1, _), (c2, _), (c3, _)) = c1 == 't' || c2 == 't' || c3 == 't'

biggestClique :: Map Computer (Set Computer) -> Set Computer
biggestClique adj =
  fst $
    Map.foldrWithKey
      ( \c frontier (res, vis) ->
          if Set.member c vis
            then (res, vis)
            else
              let bestWithCur = go (Set.singleton c) frontier
               in ( if Set.size bestWithCur > Set.size res
                      then bestWithCur
                      else res
                  , Set.union vis bestWithCur
                  )
      )
      (Set.empty, Set.empty)
      adj
  where
    go :: Set Computer -> Set Computer -> Set Computer
    go cur frontier =
      Set.foldr
        ( \c acc ->
            let res = go (Set.insert c cur) (Set.filter (\c' -> c `Set.member` fromJust (Map.lookup c' adj)) frontier)
             in if Set.size res > Set.size cur
                  then res
                  else acc
        )
        cur
        frontier

stronkTriples :: Map Computer (Set Computer) -> [(Computer, Computer, Computer)]
stronkTriples adj =
  Map.foldrWithKey
    ( \c1 cAdj acc ->
        concatMap
          ( \c2 ->
              [ (c1, c2, c3) | c3 <- Set.toList $ fromJust $ Map.lookup c2 adj, c1 `Set.member` fromJust (Map.lookup c3 adj)
              ]
          )
          cAdj
          ++ acc
    )
    []
    adj

parseInput :: [String] -> Map Computer (Set Computer)
parseInput =
  foldr
    ( \l acc ->
        let (p1, p2) = parseLine l
         in Map.insertWith Set.union p2 (Set.singleton p1) $ Map.insertWith Set.union p1 (Set.singleton p2) acc
    )
    Map.empty
  where
    parseLine (c1 : c2 : '-' : c3 : c4 : _) = ((c1, c2), (c3, c4))
    parseLine _ = error "Invalid input."
