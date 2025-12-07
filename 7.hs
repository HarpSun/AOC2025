import Debug.Trace

import Data.List (find)
import Data.Array
import Data.Array.IArray ((!?))
        
-- =============================================================
--                      Part I
-- =============================================================
type Pos = (Int, Int)

findIdx :: Ix i => (e -> Bool) -> Array i e -> i
findIdx p arr =
  case res of
    Nothing -> error "not found"
    Just (i, e) -> i
  where res = find (p . snd) (assocs arr)

-- move beam until hit splitter or reach the end
moveDown :: Array Pos Char -> Pos -> Maybe (Pos, Pos, Pos)
moveDown grid (col, row) =
  case pos of
    Nothing -> Nothing
    Just 'S' -> moveDown grid (col + 1, row)
    Just '.' -> moveDown grid (col + 1, row)
    Just '^' -> Just ((col, row - 1), (col, row + 1), (col, row))
  where pos = grid !? (col, row)

markSplit :: Array Pos Char -> Pos -> Array Pos Int -> Array Pos Int
markSplit grid pos markGrid =
  case moveDown grid pos of
    Nothing -> markGrid
    Just (p1, p2, splitP) ->
      case markGrid ! splitP of
        1 -> markGrid
        0 ->
          let g' = markSplit grid p1 (markGrid // [(splitP, 1)])
          in markSplit grid p2 g'

countSplit :: Array Pos Char -> Int
countSplit grid = sum $ elems $ markSplit grid start mem
  where start = findIdx (\x -> x == 'S') grid
        ((0, 0), (col, row)) = bounds grid
        mem = listArray ((0, 0), (col, row)) (iterate id 0)
  
parseInput :: String -> Array Pos Char
parseInput input =
  listArray ((0, 0), (col, row)) s
  where ls = lines input
        s = foldl (++) "" ls
        col = length ls - 1
        row = length (head ls) - 1


-- =============================================================
--                      Part II
-- =============================================================
count :: Array Pos Char -> Pos -> Array Pos Int -> (Int, Array Pos Int)
count grid pos mem =
  case moveDown grid pos of
    Nothing -> (1, mem)
    Just (p1, p2, splitP) ->
      case mem ! splitP of
        0 -> let (n1, mem') = count grid p1 mem
                 (n2, mem'') = count grid p2 mem'
             in (n1 + n2, mem'' // [(splitP, n1 + n2)])
        n -> (n, mem)
      

countTimeLine :: Array Pos Char -> Int
countTimeLine grid = fst $ count grid start mem
  where start = findIdx (\x -> x == 'S') grid
        ((0, 0), (col, row)) = bounds grid
        mem = listArray ((0, 0), (col, row)) (iterate id 0)

main :: IO ()
main = do
  print "day 7"
  
  contents <- readFile "7.input"
  let arr = parseInput contents
  print $ countSplit arr
  print $ countTimeLine arr
