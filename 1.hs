-- =============================================================
--                      Part I
-- =============================================================

data Direction = L | R
  deriving (Show, Eq)

type Command = (Direction, Int)

readDir :: Char -> Direction
readDir 'L' = L
readDir 'R' = R

parseInput :: String -> [Command]
parseInput input = [(readDir (head line), read (tail line) :: Int)  | line <- lines input]
    
dial :: Command -> Int -> Int
dial (L, n) state = (state - n) `mod` 100
dial (R, n) state = (state + n) `mod` 100

dialSequnce :: [Command] -> Int -> Int -> Int
dialSequnce [] state zeroCount = zeroCount
dialSequnce (c:cs) state zeroCount =
  case s of
    0 -> dialSequnce cs s (zeroCount + 1)
    _ -> dialSequnce cs s zeroCount
  where s = (dial c state)


-- =============================================================
--                      Part II
-- =============================================================
dialXCountZero :: Command -> Int -> Int -> (Int, Int)
dialXCountZero (L, n) state count
  | n > d = dialXCountZero (L, n - d) 0 (count + 1)
  | otherwise = (d - n, count)
  where d = if state == 0 then 100 else state
  
dialXCountZero (R, n) state count
  | n > d = dialXCountZero (R, n - d) 0 (count + 1)
  | otherwise = ((state + n) `mod` 100, count)
  where d = 100 - state
  
dialSequnce2 :: [Command] -> Int -> Int -> Int
dialSequnce2 [] state zeroCount = zeroCount
dialSequnce2 (c:cs) state zeroCount =
  case s of
    0 -> dialSequnce2 cs s (zeroCount + count + 1)
    _ -> dialSequnce2 cs s (zeroCount + count)
  where (s, count) = (dialXCountZero c state 0) 

  
main :: IO ()
main = do
  print "day 1"

  input <- readFile "1.input"
  let cmds = parseInput input 
  print $ dialSequnce cmds 50 0
  print $ dialSequnce2 cmds 50 0

