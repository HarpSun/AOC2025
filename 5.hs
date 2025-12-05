import Debug.Trace
import Data.List.Split (splitWhen, splitOn)
import Data.List (sortBy)

        
-- =============================================================
--                      Part I
-- =============================================================
type Range = (Int, Int)

parseInput :: String -> ([Range], [Int])
parseInput input = (rs, ns)
  where [ranges, ids] = splitWhen (== "") (lines input)
        rs = [
          (read (r !! 0) :: Int, read (r !! 1) :: Int)
          | r <- [splitOn "-" range | range <- ranges]
          ]
        ns = [read id :: Int | id <- ids]
  
compareRange :: Range -> Int -> Ordering
compareRange (begin, end) n
  | n < begin = LT
  | n > end = GT
  | otherwise = EQ

-- merge overlapped ranges
merge :: [Range] -> Range -> [Range]
merge [] r = [r]
merge ranges r
  | fst r > snd r2 = ranges ++ [r]
  | otherwise = merge (init ranges) (_merge r2 r)
  where r2 = last ranges
        _merge :: Range -> Range -> Range
        _merge r1 r2
          | snd r1 == fst r2 = (fst r1, snd r2)
          | snd r1 > fst r2 && snd r1 >= snd r2 = r1
          | otherwise = (fst r1, snd r2) -- snd r1 < snd r2
  
mergeRange :: [Range] -> [Range]
mergeRange ranges = foldl merge [] ranges
  
rangeSearch :: [Range] -> Int -> Bool
rangeSearch [] n = False
rangeSearch ranges n =
  case compareRange r n of
    LT -> rangeSearch (take mid ranges) n
    GT -> rangeSearch (drop (mid + 1) ranges) n
    EQ -> True
  where mid = (length ranges) `div` 2
        r = ranges !! mid

freshIds :: [Range] -> [Int] -> Int
freshIds ranges ids = length $ filter id [rangeSearch mergedRanges id | id <- ids]
  where sr = sortBy (\(a, _) (b, _) -> compare a b) ranges
        mergedRanges = mergeRange sr

bruteSearch :: [Range] -> Int -> Bool
bruteSearch [] n = False
bruteSearch ranges n =
  case compareRange (head ranges) n of
    EQ -> True
    _ -> bruteSearch (tail ranges) n


-- =============================================================
--                      Part II
-- =============================================================
freshIdNum :: [Range] -> Int
freshIdNum ranges = sum [snd r - fst r + 1 | r <- mergedRanges]
  where sr = sortBy (\(a, _) (b, _) -> compare a b) ranges
        mergedRanges = mergeRange sr


main :: IO ()
main = do
  print "day 5"
  contents <- readFile "5.input"
  let (ranges, ids) = parseInput contents
  
  print $ freshIds ranges ids
  print $ freshIdNum ranges
