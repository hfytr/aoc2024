{-# LANGUAGE LambdaCase #-}

import Data.Array (Array, (!))
import Data.Array qualified as Arr
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow, traceShowId, traceShowWith)

type Vec2 = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "inputs/15-small.txt"
  let (walls, boxes, startPos, instructions) = parseInput $ lines contents
      (walls2, boxes2, startPos2) = doubleInput walls boxes startPos
  -- print $ solvep1 walls boxes startPos instructions
  print $ solvep2 walls2 boxes2 startPos2 instructions

solvep2 :: Array Vec2 Bool -> Set Vec2 -> Vec2 -> [Vec2] -> Int
solvep2 walls initBoxes initPos allInstructions =
  Set.foldr (\(x, y) acc -> acc + 100 * x + y) 0 $
    fst $
      traceShowId (go initBoxes initPos allInstructions)
 where
  go :: Set Vec2 -> Vec2 -> [Vec2] -> (Set Vec2, Vec2)
  go boxes pos [] = (boxes, pos)
  go boxes pos (ii@(0, _) : itail) = case traceShowWith ("hi",pos,boxes,) (tryMove walls boxes pos ii 2) of
    Nothing -> go (traceShowId boxes) pos itail
    Just d ->
      go
        ( traceShowWith
            ("baz",d,pos +++ ii,pos +++ ii *** d *** 2,ii,pos,)
            (pushBoxes boxes (pos +++ ii *** 2) ii)
        )
        (pos +++ ii)
        itail
  go boxes pos (ii : itail) =
    if traceShow ("hi2", pos, boxes) (canMove2 boxes ii pos)
      then go (traceShowWith ("foo",pos,ii,) (moveBoxes2 boxes pos ii)) pos itail
      else go (traceShowWith ("bar",pos,) boxes) pos itail
  canMove2 :: Set Vec2 -> Vec2 -> Vec2 -> Bool
  canMove2 boxes ii pos
    | walls ! (pos +++ ii) || walls ! (pos +++ ii +++ (0, 1)) = False
    | otherwise =
        let nextBoxes =
              [pos' | i <- [-1 .. 1], let pos' = pos +++ ii +++ (0, i), Set.member pos' boxes]
         in null nextBoxes || all (canMove2 boxes ii) nextBoxes
  moveBoxes2 :: Set Vec2 -> Vec2 -> Vec2 -> Set Vec2
  moveBoxes2 boxes pos dir
    | Set.notMember pos boxes = boxes
    | otherwise =
        let nextBoxes =
              [ pos' | i <- [-1 .. 1], let pos' = pos +++ dir +++ (0, i), Set.member pos' boxes
              ]
         in foldr
              (\box acc -> moveBoxes2 acc box dir)
              boxes
              nextBoxes
  pushBoxes :: Set Vec2 -> Vec2 -> Vec2 -> Set Vec2
  pushBoxes boxes pos dir
    | snd pos < 0 = error ""
    | Set.notMember pos boxes = boxes
    | otherwise =
        let nextPos = traceShowWith (pos,boxes,Set.notMember pos boxes,) (pos +++ dir *** 2)
         in moveBox pos nextPos $ pushBoxes boxes nextPos dir

solvep1 :: Array Vec2 Bool -> Set Vec2 -> Vec2 -> [Vec2] -> Int
solvep1 walls initBoxes initPos allInstructions =
  Set.foldr (\(x, y) acc -> acc + 100 * x + y) 0 $
    fst $
      go initBoxes initPos allInstructions
 where
  go :: Set Vec2 -> Vec2 -> [Vec2] -> (Set Vec2, Vec2)
  go boxes pos [] = (boxes, pos)
  go boxes pos (ii : itail) = case tryMove walls boxes pos ii 1 of
    Nothing -> go boxes pos itail
    Just d -> go (moveBox (pos +++ ii) (pos +++ ii *** d) boxes) (pos +++ ii) itail

tryMove :: Array Vec2 Bool -> Set Vec2 -> Vec2 -> Vec2 -> Int -> Maybe Int
tryMove walls boxes pos dir offset
  | walls ! (pos +++ dir *** offset) = Nothing
  | Set.notMember (pos +++ dir *** offset) boxes = Just 1
  | otherwise = (+ 1) <$> tryMove walls boxes (pos +++ dir) dir offset

moveBox :: Vec2 -> Vec2 -> Set Vec2 -> Set Vec2
moveBox p0 p1 boxes
  | Set.member p0 boxes = Set.insert p1 $ Set.delete p0 boxes
  | otherwise = boxes

doubleInput ::
  Array Vec2 Bool ->
  Set Vec2 ->
  Vec2 ->
  (Array Vec2 Bool, Set Vec2, Vec2)
doubleInput walls boxes (startx, starty) =
  let (_, (width, height)) = Arr.bounds walls
   in ( Arr.array
          ((0, 0), (width, height * 2 + 1))
          ( concatMap
              (\idx@(i, j) -> [((i, j * 2), walls ! idx), ((i, j * 2 + 1), walls ! idx)])
              (Arr.indices walls)
          )
      , Set.map (\(x, y) -> (x, y * 2)) boxes
      , (startx, starty * 2)
      )

parseInput :: [String] -> (Array Vec2 Bool, Set Vec2, Vec2, [Vec2])
parseInput list =
  let (gridRaw, _ : instructionsRaw) = splitAt (fromJust $ elemIndex "" list) list
      (gridList, boxes, startPos) =
        foldr
          ( \(i, s) (accGrid, accBoxes, accStart) ->
              foldr
                ( \(j, c) (accGrid', accBoxes', accStart') -> case c of
                    '.' -> (((i, j), False) : accGrid', accBoxes', accStart')
                    '#' -> (((i, j), True) : accGrid', accBoxes', accStart')
                    'O' -> (((i, j), False) : accGrid', Set.insert (i, j) accBoxes', accStart')
                    '@' -> (((i, j), False) : accGrid', accBoxes', (i, j))
                    _ -> error "invalid char in grid"
                )
                (accGrid, accBoxes, accStart)
                (zip [0 ..] s)
          )
          ([], Set.empty, (0, 0))
          (zip [0 ..] gridRaw)
      instructions =
        map
          ( \case
              '<' -> (0, -1)
              '^' -> (-1, 0)
              '>' -> (0, 1)
              'v' -> (1, 0)
              _ -> error "invalid char in instructions"
          )
          (concat instructionsRaw)
   in ( Arr.array
          ((0, 0), (length gridRaw - 1, length (head gridRaw) - 1))
          gridList
      , boxes
      , startPos
      , instructions
      )

infixl 7 ***
(***) :: Vec2 -> Int -> Vec2
(***) (x, y) c = (x * c, y * c)

infixl 6 +++
(+++) :: Vec2 -> Vec2 -> Vec2
(+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
