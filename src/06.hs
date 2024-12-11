import Data.Bits ((.&.), (.|.))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Word (Word8)

type BoolMat = (Int, Int, Set (Int, Int))
type VisMap = Map (Int, Int) Word8
data Direction = N | S | E | W

instance Show Direction where
  show d = case d of
    N -> "N"
    S -> "S"
    E -> "E"
    W -> "W"

main :: IO ()
main = do
  contents <- readFile "inputs/6-actual.txt"
  let (board, startPos) = parseInput (lines contents)
      (uneditedVis, p1Sol) = solvep1 board startPos
      p2Sol = solvep2 board startPos uneditedVis
  putStrLn "Part I:"
  print p1Sol
  putStrLn "Part II:"
  print p2Sol

solvep2 :: BoolMat -> (Int, Int, Direction) -> VisMap -> Int
solvep2 (width, height, board) (x, y, d) uneditedVis =
  length $
    filter checkAddObstacle $
      filter
        ((x, y) /=)
        [(mod i height, div i width) | i <- [0 .. width * height - 1]]
 where
  vis = makeStartingVis (height, width)
  checkAddObstacle (x', y') =
    visContainsCoord uneditedVis (x', y')
      && checkLoop (width, height, S.insert (x', y') board) (x, y, d) vis

checkLoop :: BoolMat -> (Int, Int, Direction) -> VisMap -> Bool
checkLoop board (x, y, d) vis
  | not (checkBounds board (x, y)) = False
  | visContains vis (x, y, d) = True
  | otherwise =
      let newPos = updatePos board (x, y, d)
      in checkLoop board newPos $ updateVis vis (x, y, d)

solvep1 :: BoolMat -> (Int, Int, Direction) -> (VisMap, Int)
solvep1 (width, height, board) (x, y, d) =
  let endSet =
        traverseBoard (width, height, board) (x, y, d) $
          makeStartingVis (width, height)
  in (endSet, length $ filter (\(_, e) -> e > 0) $ M.toList endSet)

traverseBoard :: BoolMat -> (Int, Int, Direction) -> VisMap -> VisMap
traverseBoard board (x, y, d) vis
  | not (checkBounds board (x, y)) = vis
  | otherwise =
      let newPos = updatePos board (x, y, d)
      in traverseBoard board newPos $ updateVis vis (x, y, d)

updatePos ::
  BoolMat -> (Int, Int, Direction) -> (Int, Int, Direction)
updatePos (w, h, board) (x, y, d) =
  let (dx, dy) = directionAsDelta d
  in -- if invalid next, return. its handled by traversal
     if checkBounds (w, h, board) (x + dx, y + dy) && S.member (x + dx, y + dy) board
      then updatePos (w, h, board) (x, y, rotateClockwise d)
      else (x + dx, y + dy, d)

updateVis :: VisMap -> (Int, Int, Direction) -> VisMap
updateVis vis (x, y, d) = M.adjust (directionAsBitmask d .|.) (x, y) vis

directionAsBitmask :: Direction -> Word8
directionAsBitmask d = case d of
  N -> 0b1000
  E -> 0b0100
  S -> 0b0010
  W -> 0b0001

directionAsDelta :: Direction -> (Int, Int)
directionAsDelta d = case d of
  N -> (1, 0)
  E -> (0, 1)
  S -> (-1, 0)
  W -> (0, -1)

rotateClockwise :: Direction -> Direction
rotateClockwise d = case d of
  N -> W
  W -> S
  S -> E
  E -> N

checkBounds :: BoolMat -> (Int, Int) -> Bool
checkBounds (width, height, _) (x, y) = x >= 0 && x < width && y >= 0 && y < height

visContainsCoord :: VisMap -> (Int, Int) -> Bool
visContainsCoord vis k = (fromJust . M.lookup k) vis > 0

visContains :: VisMap -> (Int, Int, Direction) -> Bool
visContains vis (x, y, d) = directionAsBitmask d .&. (fromJust . M.lookup (x, y)) vis > 0

makeStartingVis :: (Int, Int) -> VisMap
makeStartingVis (x, y) =
  M.fromList
    [((xi, yi), 0) | yi <- [0 .. y - 1], xi <- [0 .. x - 1]]

parseInput :: [String] -> (BoolMat, (Int, Int, Direction))
parseInput list = (getObstacles, getStarting)
 where
  getObstacles :: BoolMat
  getObstacles =
    let width = length list
        height = (length . head) list
    in ( width
       , height
       , S.fromList
          ( concatMap
              ( \(i, s) -> map (\(j, _) -> (i, j)) (filter (\(_, c) -> c == '#') (zip [0 ..] s))
              )
              (zip [0 ..] list)
          )
       )

  getStarting :: (Int, Int, Direction)
  getStarting =
    let startingRow = snd $ head $ filter (\(row, _) -> '^' `elem` row) (zip list [0 ..])
        startingCol = snd $ head $ filter (\(c, _) -> c == '^') (zip (list !! startingRow) [0 ..])
    in (startingRow, startingCol, S)
