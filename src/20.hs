import Data.Array (Array, (!))
import Data.Array qualified as Arr
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)

data Cheat = Unused | Used deriving (Show, Eq, Ord)

type Vec2 = (Int, Int)

main :: IO ()
main = do
  contents <- readFile "inputs/20-actual.txt"
  let input = parseInput $ lines contents
  putStrLn "Part I:"
  print $ solve input 2
  putStrLn "Part II:"
  print $ solve input 20

solve :: (Vec2, Vec2, Array Vec2 Bool) -> Int -> Int
solve (start, end, grid) cheatDist = length $ filter ((>= 100) . (noCheatDist -)) $ go start (-1, -1) 0 Unused
 where
  go :: Vec2 -> Vec2 -> Int -> Cheat -> [Int]
  go p _ d Used = map (d +) $ maybeToList $ Map.lookup p noCheats
  go p pOld d Unused
    | p == end = []
    | otherwise =
        concatMap
          ( \(p', cheatLen) ->
              map (d + cheatLen +) $
                maybeToList $
                  Map.lookup p' noCheats
          )
          (possibleCheats p)
          ++ go
            (nextOnTrack p pOld)
            p
            (d + 1)
            Unused

  possibleCheats :: Vec2 -> [(Vec2, Int)]
  possibleCheats (x, y) =
    [ ((dx + x, dy + y), abs dx + abs dy) | dx <- [-cheatDist .. cheatDist], let dyMag = cheatDist - abs dx, dy <- [-dyMag .. dyMag], abs dy + abs dx > 1, checkBounds (x + dx, y + dy) && not (grid ! (dx + x, dy + y)), Map.member (dx + x, dy + y) noCheats
    ]

  noCheatDist = Map.size noCheats
  noCheats = snd $ genNoCheats start (-1, -1)
  genNoCheats :: Vec2 -> Vec2 -> (Int, Map Vec2 Int)
  genNoCheats p pOld
    | p == end = (0, Map.singleton p 0)
    | otherwise =
        let (childDist, childRes) = genNoCheats (nextOnTrack p pOld) p
         in (childDist + 1, Map.insert p (childDist + 1) childRes)
  nextOnTrack p pOld =
    head
      [p' | v <- dirs, let p' = v +++ p, p' /= pOld, checkBounds p', not (grid ! p')]

  checkBounds (x, y) = x >= 0 && x <= maxx && y >= 0 && y <= maxy
  (_, (maxx, maxy)) = Arr.bounds grid

dirs :: [Vec2]
dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]

parseInput :: [String] -> (Vec2, Vec2, Array Vec2 Bool)
parseInput list =
  let
    (gridList, start, end) =
      foldr
        ( \(i, str) (acc, s, e) ->
            foldr
              ( \(j, c) (acc', s', e') -> case c of
                  '#' -> (((i, j), True) : acc', s', e')
                  'S' -> (((i, j), False) : acc', (i, j), e')
                  'E' -> (((i, j), False) : acc', s', (i, j))
                  _ -> (((i, j), False) : acc', s', e')
              )
              (acc, s, e)
              (zip [0 ..] str)
        )
        ([], (-1, -1), (-1, -1))
        (zip [0 ..] list)
   in
    ( start
    , end
    , Arr.array ((0, 0), (length list - 1, length (head list) - 1)) gridList
    )

(+++) :: Vec2 -> Vec2 -> Vec2
(+++) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
