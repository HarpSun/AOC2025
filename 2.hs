import Data.List.Split (splitOn)


-- =============================================================
--                      Part I
-- =============================================================
type Range = (Int, Int)

parseInput :: String -> [Range]
parseInput input =
  fmap (\x -> (read (x !! 0) :: Int, read (x !! 1) :: Int)) ranges
  where ls = splitOn "," input
        ranges = [splitOn "-" r | r <- ls]

  
isRepeated :: Int -> Bool
isRepeated n
  | odd len = False
  | otherwise = take (len `div` 2) s == drop (len `div` 2) s
  where s = show n
        len = length s

sumInvalid :: Range -> Int
sumInvalid (begin, end) = sum $ filter isRepeated [i | i <- [begin..end]]

sumInvalids :: [Range] -> Int
sumInvalids ls = sum $ fmap sumInvalid ls

-- =============================================================
--                      Part II
-- =============================================================
hasPattern :: String -> String -> Bool
hasPattern "" p = True
hasPattern str p
  | s == p = hasPattern (drop (length p) str) p
  | otherwise = False
  where s = take (length p) str
  
isRepeated2 :: Int -> Bool
isRepeated2 n = any (hasPattern s) [(take i s)| i <- [1..end]]
  where s = show n
        end = length s `div` 2
        
sumInvalid2 :: Range -> Int
sumInvalid2 (begin, end) = sum $ filter isRepeated2 [i | i <- [begin..end]]

sumInvalids2 :: [Range] -> Int
sumInvalids2 ls = sum $ fmap sumInvalid2 ls

main :: IO ()
main = do
  print "day 2"
  contents <- readFile "2.input"
  print $ sumInvalids (parseInput contents)
  print $ sumInvalids2 (parseInput contents)
