import Debug.Trace

import Data.List.Split (splitOn)
import Data.List (sortBy, intercalate)
import Data.Array

-- =============================================================
--                      Part I
-- =============================================================
type Pos = (Int, Int)
        
area :: Pos -> Pos -> Int
area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

areaMap :: [Pos] -> [((Pos, Pos), Int)]
areaMap [] = []
areaMap posList = maps ++ (areaMap (tail posList))
  where p = head posList
        maps = [((p, pos), area p pos) | pos <- (tail posList)]

largestArea :: [Pos] -> Int
largestArea posList = area
  where map = areaMap posList
        sortedMap = sortBy (\(_, d1) (_, d2) -> (flip compare) d1 d2) map
        (_, area) = head sortedMap
  
parseInput :: String -> [Pos]
parseInput input =
  [(read (e !! 0) :: Int,
    read (e !! 1) :: Int)
  | e <- ls]
  where ls = [splitOn "," line | line <- lines input]

main :: IO ()
main = do
  print "day 9"

  let posList = parseInput s
  print $ largestArea posList
