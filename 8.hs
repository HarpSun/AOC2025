import Debug.Trace
import Data.List.Split (splitOn)
import Data.List (sortBy, sort, findIndex)

updateList :: [a] -> Int -> a -> [a]
updateList [] i v = []
updateList ls i v
  | i == 0 = v : (tail ls)
  | otherwise = (head ls) : (updateList (tail ls) (i - 1) v)

removeList :: [a] -> Int -> [a]
removeList [] i = []
removeList ls i
  | i == 0 = tail ls
  | otherwise = (head ls) : (removeList (tail ls) (i - 1))
  
-- =============================================================
--                      Part I
-- =============================================================
type Pos = (Int, Int, Int)
type Circuit = [Pos]

distance :: Pos -> Pos -> Float
distance (x1, y1, z1) (x2, y2, z2) = sqrt $ fromIntegral v
  where v = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

distanceMap :: [Pos] -> [((Pos, Pos), Float)]
distanceMap [] = []
distanceMap posList = maps ++ (distanceMap (tail posList))
  where p = head posList
        maps = [((p, pos), distance p pos) | pos <- (tail posList)]
  
junction :: Pos -> Pos -> [Circuit] -> [Circuit]
junction p1 p2 [] = [[p1, p2]]
junction p1 p2 circuits = 
  case (idx1, idx2) of
    (Nothing, Nothing) -> [p1, p2]:circuits
    (Nothing, Just i) -> updateList circuits i (p1:(circuits !! i))
    (Just i, Nothing) -> updateList circuits i (p2:(circuits !! i))
    (Just i1, Just i2) ->
      if i1 == i2
      then circuits
      else
      removeList (updateList circuits i1 (circuits !! i1 ++ circuits !! i2)) i2
  where
    idx1 = findIndex (\x -> p1 `elem` x) circuits
    idx2 = findIndex (\x -> p2 `elem` x) circuits

makeConnect :: [((Pos, Pos), Float)] -> [Circuit] -> Int -> [Circuit]
makeConnect map circuits 0 = circuits
makeConnect map circuits n =
  makeConnect (tail map) circuits' (n - 1)
  where ((p1, p2), _) = head map
        circuits' = junction p1 p2 circuits

threeLargestCircuit :: [Pos] -> Int -> Int
threeLargestCircuit posList n =
  foldl (*) 1 (take 3 (sortBy (flip compare) (fmap length circuits)))
  where map = distanceMap posList
        sortedMap = sortBy (\(_, d1) (_, d2) -> compare d1 d2) map
        circuits = makeConnect sortedMap [] n

parseInput :: String -> [Pos]
parseInput input =
  [(read (e !! 0) :: Int,
    read (e !! 1) :: Int,
    read (e !! 2) :: Int)
  | e <- ls]
  where ls = [splitOn "," line | line <- lines input]


-- =============================================================
--                      Part II
-- =============================================================
makeConnect2 :: [((Pos, Pos), Float)] -> [Circuit] -> Int -> (Pos, Pos)
makeConnect2 map circuits n
  | length map == 0 = error "can't connect all"
  | otherwise =
    let ((p1, p2), _) = head map
        circuits' = junction p1 p2 circuits
    in
      if (length circuits' == 1) &&
         ((length $ head circuits') == n)
      then (p1, p2)
      else
        makeConnect2 (tail map) circuits' n
        
mulLastTwo :: [Pos] -> Int -> Int
mulLastTwo posList n = x1 * x2
  where map = distanceMap posList
        sortedMap = sortBy (\(_, d1) (_, d2) -> compare d1 d2) map
        ((x1, _, _), (x2, _, _)) = makeConnect2 sortedMap [] n
        
  
main :: IO ()
main = do
  print "day 8"
  contents <- readFile "8.input"
  let posList = parseInput contents
  print $ threeLargestCircuit posList 10

  print $ mulLastTwo (parseInput contents) 1000
  
