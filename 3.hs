import Data.List (findIndices)
import Data.Char (digitToInt)

-- =============================================================
--                      Part I
-- =============================================================
parseInput :: String -> [[Int]]
parseInput input = [
  fmap (digitToInt . (line !!)) [i | i <- [0..(length line - 1)]]
  | line <- lines input
  ]

maxFromList :: [Int] -> Int
maxFromList ls
  | length ls == 1 = head ls
  | otherwise = max (head ls) (maxFromList (tail ls))

maxJoltage :: [Int] -> Int
maxJoltage ls
  | length ls <= 1 = error "bank must have at least 2 element"
  | otherwise = 
  let ls' = take (length ls - 1) ls
      n = maxFromList ls'
      indices = findIndices (\x -> x == n) ls'
      ls2 = [maxFromList $ drop (i + 1) ls | i <- indices]
    in maxFromList [n * 10 + i | i <- ls2]

sumMaxJoltage :: [[Int]] -> Int
sumMaxJoltage ls = sum $ fmap maxJoltage ls


-- =============================================================
--                      Part II
-- =============================================================
maxJoltage2 :: [Int] -> Int -> Int
maxJoltage2 ls left
  | left == 0 = maxFromList ls
  | otherwise =
    let ls' = take (length ls - left) ls
        n = maxFromList ls'
        indices = findIndices (\x -> x == n) ls'
        ls2 = [maxJoltage2 (drop (i + 1) ls) (left - 1)  | i <- indices]
    in maxFromList [n * 10 ^ left + i | i <- ls2]

sumMaxJoltage2 :: [[Int]] -> Int
sumMaxJoltage2 ls = sum [maxJoltage2 l 11 | l <- ls]


main :: IO ()
main = do
  print "day 3"
  contents <- readFile "3.input"
  let ls = parseInput contents
  
  print $ sumMaxJoltage ls
  print $ sumMaxJoltage2 ls

          
