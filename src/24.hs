import Data.Bits (shift, xor, (.|.))
import Data.List (elemIndex, partition, sortOn, tails)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Debug.Trace

main :: IO ()
main = do
  contents <- readFile "inputs/24-actual.txt"
  let (vars, relations) = parseInput $ lines contents
  putStrLn "Part I:"
  print $ joinBits 'z' $ solve vars $ Map.toList relations
  putStrLn "Part II:"

solve :: Map String Bool -> [(String, (String, String, Bool -> Bool -> Bool))] -> Map String Bool
solve vars relations =
  let (computable, inComputable) =
        partition
          ( \(_, (a, b, _)) ->
              Map.member a vars && Map.member b vars
          )
          relations
      newVars =
        foldr
          ( \(c, (a, b, op)) acc ->
              Map.insert
                c
                (fromJust (Map.lookup a vars) `op` fromJust (Map.lookup b vars))
                acc
          )
          vars
          computable
   in if null computable then vars else solve newVars inComputable

joinBits :: Char -> Map String Bool -> Int
joinBits c vars = foldr (\(c' : _, val) acc -> if c' == c then shift acc 1 .|. fromEnum val else acc) 0 $ Map.toAscList vars

parseInput :: [String] -> (Map String Bool, Map String (String, String, Bool -> Bool -> Bool))
parseInput list =
  let blankLine = fromJust $ elemIndex "" list
      initAssignments =
        Map.fromList
          [ (init varName, toEnum (read value))
          | line <- take blankLine list
          , let [varName, value] = words line
          ]
      relations =
        Map.fromList
          $ map
            ( \l ->
                let [a, opRaw, b, _, c] = words l
                    op = case opRaw of
                      "XOR" -> xor
                      "AND" -> (&&)
                      "OR" -> (||)
                      _ -> error "invalid operand"
                 in (c, (a, b, op))
            )
          $ traceShowWith groupRelations
          $ tails list !! (blankLine + 1)
   in (initAssignments, relations)

groupRelations :: [String] -> String
groupRelations list =
  let
    input = map (\s -> let [v1, op, v2, _, out] = words s in (v1, v2, out, op)) list
    directXors :: [(Int, String)]
    directXors = traceShowId $ sortOn fst $ map (\(_ : n, _, out, _) -> (read n, out)) $ filter (\(c : _, _, _, op) -> (c == 'x' || c == 'y') && op == "XOR") input
    directAnds :: [(Int, String)]
    directAnds = sortOn fst $ map (\(_ : n, _, out, _) -> (read n, out)) $ filter (\(c : _, _, _, op) -> (c == 'x' || c == 'y') && op == "AND") input
    relations = traceShowId $ Map.fromList $ map (\(v1, v2, out, op) -> ((v1, v2, op), out)) input
    look :: String -> String -> String -> Maybe String
    look a b op = case Map.lookup (a, b, op) relations of
      Just res -> Just res
      Nothing -> Map.lookup (b, a, op) relations
    carryBits =
      foldl
        ( \(carry : ctail, digReses, carries') ((i, xorRes), (_, andRes)) ->
            let
              digRes = look xorRes carry "XOR"
              nextCarry' = look xorRes carry "AND"
              nextCarry = nextCarry' >>= (\x -> look x andRes "OR")
             in
              traceShow (xorRes, andRes, carry, digRes, nextCarry, nextCarry') (fromJust nextCarry : carry : ctail, fromJust digRes : digReses, fromJust nextCarry' : carries')
        )
        (["drq"], [], [])
        (zip (tail directXors) (tail directAnds))
   in
    traceShow carryBits ""

-- ctg,dmh,dvq,rpb,rpv,z11,z31,z38
