import Debug.Trace
import Data.Array
import Data.Array.IArray ((!?))


-- =============================================================
--                      Part I
-- =============================================================
type Pos = (Int, Int)

parseInput :: String -> Array Pos Char
parseInput input = listArray ((0, 0), (col, row)) (foldl (++) "" ls)
  where ls = lines input
        col = length ls - 1
        row = length (head ls) - 1

adjacentPaper :: Array Pos Char -> Pos -> Int
adjacentPaper arr (col, row) = length $ [p | p <- adjacents, arr !? p == Just '@']
  where up = (col, row - 1)
        down = (col, row + 1)
        left = (col - 1, row)
        right = (col + 1, row)
        upLeft = (col - 1, row - 1)
        upRight = (col + 1, row - 1)
        downLeft = (col - 1, row + 1)
        downRight = (col + 1, row + 1)
        adjacents = [up, down, left, right, upLeft, upRight, downLeft, downRight]

numOfAccessdPaper :: Array Pos Char -> Int
numOfAccessdPaper arr = length $ filter (\x -> x < 4) [adjacentPaper arr i | i <- indices arr, arr ! i == '@']


-- =============================================================
--                      Part II
-- =============================================================
accessdPaper :: Array Pos Char -> [Pos]
accessdPaper arr = [i | i <- indices arr, arr ! i == '@' && adjacentPaper arr i < 4]

numOfRemovedPaper :: Array Pos Char -> Int
numOfRemovedPaper arr
  | length pos == 0 = 0
  | otherwise =
    let arr' = arr // [(p, 'x') | p <- pos]
        in (length pos) + numOfRemovedPaper arr'
  where pos = accessdPaper arr


main :: IO ()
main = do
  print "day4"
  contents <- readFile "4.input"
  let arr = parseInput contents
  print $ numOfAccessdPaper arr

  print $ numOfRemovedPaper arr
