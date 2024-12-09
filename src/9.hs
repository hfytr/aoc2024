import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq

-- File (length, idNum)
data Block = Free Int | File (Int, Int) deriving (Show)

main :: IO ()
main = do
  contents <- readFile "inputs/9-actual.txt"
  let input = parseInput contents
  putStrLn "Part I:"
  print $ checkSum $ byBlock input
  putStrLn "Part II:"
  print $ checkSum $ byFile input

checkSum :: Seq Block -> Int
checkSum blocks = go blocks 0
 where
  go :: Seq Block -> Int -> Int
  go Seq.Empty _ = 0
  go (Free len :<| bTail) i = go bTail (i + len)
  go (File (len, idNum) :<| bTail) i = idNum * sumConsectutive i (i + len) + go bTail (i + len)

sumConsectutive :: Int -> Int -> Int
sumConsectutive a b = div (b * (b - 1)) 2 - div (a * (a - 1)) 2

byFile :: Seq Block -> Seq Block
byFile blocks@(_ :|> File _) = go blocks blocks
 where
  insertBlock :: Block -> Seq Block -> Bool -> Seq Block
  insertBlock (Free _) _ _ = error "Tried to insert Free block."
  insertBlock _ Seq.Empty _ = Seq.Empty
  insertBlock f1@(File (len1, id1)) (f2 :<| bTail) shouldFree =
    case f2 of
      File (len2, id2)
        | id1 == id2 -> (if shouldFree then Free len2 else f2) :<| bTail
        | otherwise -> f2 :<| insertBlock f1 bTail shouldFree
      Free len2
        | len2 >= len1 && not shouldFree ->
            f1 :<| Free (len2 - len1) :<| insertBlock f1 bTail True
        | otherwise -> Free len2 :<| insertBlock f1 bTail shouldFree

  go :: Seq Block -> Seq Block -> Seq Block
  go blocks' Seq.Empty = blocks'
  go blocks' (bHead :|> Free _) = go blocks' bHead
  go blocks' (bHead :|> f@(File _)) = go (insertBlock f blocks' False) bHead
byFile (bHead :|> Free _) = byFile bHead
byFile _ = Seq.Empty

byBlock :: Seq Block -> Seq Block
byBlock Seq.Empty = Seq.Empty
byBlock (f@(File _) :<| bTail) = f :<| byBlock bTail
byBlock (bHead :|> File (0, _)) = byBlock bHead
byBlock (Free 0 :<| bTail) = byBlock bTail
byBlock (bHead :|> Free _) = byBlock bHead
byBlock (Free freeLen :<| (bMid :|> File (fileLen, idNum))) =
  let insertLen = min freeLen fileLen
  in File (insertLen, idNum)
      :<| byBlock
        ( Free (freeLen - insertLen)
            :<| (bMid :|> File (fileLen - insertLen, idNum))
        )
byBlock _ = error "Reached invalid state."

parseInput :: String -> Seq Block
parseInput s = Seq.fromList $ go (init s) True 0
 where
  go :: String -> Bool -> Int -> [Block]
  go [] _ _ = []
  go (si : stail) False idNum = Free (read [si]) : go stail True idNum
  go (si : stail) True idNum = File (read [si], idNum) : go stail False (idNum + 1)
