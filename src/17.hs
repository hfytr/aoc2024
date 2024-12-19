import Data.Bits (Bits (xor), shiftL, shiftR)
import Data.List (tails)
import Data.List.Split (splitOn)
import Data.Maybe (maybeToList)
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vec

main :: IO ()
main = do
  contents <- readFile "inputs/17-actual.txt"
  let input@(_, b, c, program) = parseInput $ lines contents
  putStrLn "Part I:"
  print $ runProgram input
  putStrLn "Part II:"
  print $ solvep2 (b, c, program)

solvep2 :: (Int, Int, Vector Int) -> Int
solvep2 (b, c, program) =
  head $
    filter (\a -> fst (runProgram (a, b, c, program)) == Vec.toList program) $
      go 0
 where
  go :: Int -> [Int]
  go i
    | i >= Vec.length program = [0]
    | otherwise =
        concatMap (\a -> map (a `shiftL` 3 +) $ getABits (program ! i) a) $ go (i + 1)
  getABits :: Int -> Int -> [Int]
  getABits p a =
    filter
      (\aBits -> p == head (fst $ runProgram (a `shiftL` 3 + aBits, b, c, inLoop)))
      [0 .. 7]
  inLoop = Vec.slice 0 (Vec.length program - 2) program

runProgram :: (Int, Int, Int, Vector Int) -> ([Int], (Int, Int, Int))
runProgram (a0, b0, c0, program) = go a0 b0 c0 0
 where
  go :: Int -> Int -> Int -> Int -> ([Int], (Int, Int, Int))
  go aOld bOld cOld iOld
    | iOld >= Vec.length program = ([], (aOld, bOld, cOld))
    | otherwise =
        let (a, b, c, i, o) = processInstruction aOld bOld cOld iOld
            (out, (a', b', c')) = go a b c i
         in (maybeToList o ++ out, (a', b', c'))
  processInstruction ::
    Int -> Int -> Int -> Int -> (Int, Int, Int, Int, Maybe Int)
  processInstruction a b c i = case program ! i of
    1 -> (a, xor b (program ! (i + 1)), c, i + 2, Nothing)
    2 -> (a, processCombo a b c (program ! (i + 1)) `mod` 8, c, i + 2, Nothing)
    3 ->
      if a == 0
        then (a, b, c, i + 2, Nothing)
        else (a, b, c, program ! (i + 1), Nothing)
    4 -> (a, xor b c, c, i + 2, Nothing)
    5 -> (a, b, c, i + 2, Just (processCombo a b c (program ! (i + 1)) `mod` 8))
    0 -> (a `shiftR` processCombo a b c (program ! (i + 1)), b, c, i + 2, Nothing)
    6 -> (a, a `shiftR` processCombo a b c (program ! (i + 1)), c, i + 2, Nothing)
    7 -> (a, b, a `shiftR` processCombo a b c (program ! (i + 1)), i + 2, Nothing)
    _ -> error "Invalid op code."
  processCombo a _ _ 4 = a
  processCombo _ b _ 5 = b
  processCombo _ _ c 6 = c
  processCombo _ _ _ 7 = error "Invalid combo operand 7."
  processCombo _ _ _ o = o

parseInput :: [String] -> (Int, Int, Int, Vector Int)
parseInput (aline : bline : cline : "" : programLine : _) =
  ( parseReg aline
  , parseReg bline
  , parseReg cline
  , Vec.fromList $ map read $ splitOn "," $ words programLine !! 1
  )
 where
  parseReg line = read $ tails line !! length "Register A: "
parseInput _ = error "invalid input"

{-
0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5
bst A -- B = A mod 8
bxl 3 -- B = (A mod 8) xor 3
cdv B -- C = A / 2 ^ B
bxc   -- B = (A / 2 ^ B) ^ B
bxl 1 -- B = B ^ 1
adv 3 -- A /= 3
out B
jnz 0
-}
