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

-- =============================================================
--                      Part II
-- =============================================================
compressCoord :: [Pos] -> ((Int, Int), [Pos], [Pos] -> [Pos])
compressCoord coord = ((cols, rows), transform coord, transform)
  where xs = Set.fromList $ [x | (x, _) <- coord]
        ys = Set.fromList $ [y | (_, y) <- coord]
        (cols, rows) = (length xs, length ys)
        xMap = Map.fromList $ zip (Set.toList xs) (iterate (+1) 0)
        yMap = Map.fromList $ zip (Set.toList ys) (iterate (+1) 0)
        lookupOrError :: (Ord a, Show a) => a -> Map a b -> b
        lookupOrError k map = case Map.lookup k map of
                                Nothing -> error $ "can't found k:" ++ show k
                                Just a -> a
        transform :: [Pos] -> [Pos]
        transform ps = [(lookupOrError x xMap, lookupOrError y yMap) |(x, y) <- ps]

mkMap :: (Int, Int) -> [Pos] -> Array Pos Char
mkMap (cols, rows) pos = map // [((y, x), '#') | (x, y) <- pos]
  where map = listArray ((0, 0), (rows, cols)) (iterate id '.')

-- using raycast check intersect
pointIn :: (Int, Int) -> [Pos] -> Pos -> Bool
pointIn (cols, rows) poses (x, y)
  | any id [ (y == y1 && y == y2 && x >= x1 && x <= x2) ||
             (x == x1 && x == x2 && y >= y1 && y <= y2)
           | [(x1, y1), (x2, y2)] <- edges] = True
  | otherwise = odd $ length $ filter id [intersect (x', y) edges | x' <- [x..cols]]
  where poses' = (poses ++ [head poses])
        edges = [sort [poses' !! i, poses' !! (i+1)] | i <- [0..length poses'-2]]
        intersect :: Pos -> [[Pos]] -> Bool
        intersect (x, y) edges = any id [ x == x1 && y >= y1 && y <= y2
                                        | [(x1, y1), (x2, y2)] <- edges, y1 /= y2 && (y1 < y || y2 < y)] -- handle edge case when it cross vertex of edge

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p [] = []
dropUntil p (x:xs) = if p x then [x] else dropUntil p xs

type Visited = [Pos]
rasterize :: (Int, Int) -> [Pos] -> [Pos]
rasterize (cols, rows) poses = res ++ edges
  where edges = foldl connect [] (poses ++ [head poses])
        p = head $ dropUntil (pointIn (cols, rows) poses)
            [(x, y) | x <- [0..cols], y <- [0..rows], not $ (x, y) `elem` edges]
        (res, _) = runState (fill edges p) []
        
        connect :: [Pos] -> Pos -> [Pos]
        connect [] p = [p]
        connect poses (x1, y1)
          | x1 == x2 && y1 > y2 = poses' ++ [(x1, y) | y <- [y2..y1]]
          | x1 == x2 && y1 < y2 = poses' ++ [(x1, y) | y <- [y2,y2-1..y1]]
          | y1 == y2 && x1 > x2 = poses' ++ [(x, y1) | x <- [x2..x1]]
          | y1 == y2 && x1 < x2 = poses' ++ [(x, y1) | x <- [x2,x2-1..x1]]
          | otherwise = error $ "not aligned" ++ show (x1, y1) ++ show (x2, y2)
          where (x2, y2) = (last poses)
                poses' = init poses

        -- pure functinal dfs, can I implment it without using state monad?
        fill :: [Pos] -> Pos -> State Visited [Pos]
        fill edges (x, y)
          | (x, y) `elem` edges = return []
          | otherwise = do
              visited <- get
              let dirs = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
              modify' ((x, y):)
              ps <- mapM (fill edges) (filter (\x -> not $ (x `elem` visited || x `elem` edges)) dirs)
              let res = (x, y) : foldl (++) [] ps
              return res
        

splitList :: Int -> [a] -> [[a]]
splitList n [] = []
splitList n ls = (take n ls) : (splitList n (drop n ls))

showMap :: Array Pos Char -> String
showMap map = intercalate "\n" (splitList (row + 1) chars) ++ "\n"
  where ((0, 0), (row, col)) = bounds map
        chars = elems map

inPolygon :: Array Pos Char -> Pos -> Pos -> Bool
inPolygon map (x1, y1) (x2, y2) = all (inside map) edges
  where edges = [[(x1, y1), (x2, y1)], [(x2, y1), (x2, y2)], [(x2, y2), (x1, y2)], [(x1, y2), (x1, y1)]]
        inside :: Array Pos Char -> [Pos] -> Bool
        inside map ps
          | x1 == x2 = all (\x -> x == '#') (fmap (map !) [(y, x1) | y <- [y1..y2]])
          | y1 == y2 = all (\x -> x == '#') (fmap (map !) [(y1, x) | x <- [x1..x2]])
          where [(x1, y1), (x2, y2)] = sort ps

areaMap2 :: [Pos] -> Array Pos Char -> ([Pos] -> [Pos]) -> [((Pos, Pos), Int)]
areaMap2 [] map transform = []
areaMap2 posList map transform = maps ++ (areaMap2 (tail posList) map transform)
  where p = head posList
        maps = [((p, pos), area p pos) |
                pos <- (tail posList), let [p', pos'] = transform [p, pos] in inPolygon map p' pos']

largestArea2 :: [Pos] -> Int
largestArea2 posList = area
  where (size, poses, transform) = compressCoord posList
        map = mkMap size $ rasterize size poses
        areaMap = areaMap2 posList map transform
        sortedMap = sortBy (\(_, d1) (_, d2) -> (flip compare) d1 d2) areaMap
        (_, area) = head sortedMap

main :: IO ()
main = do
  print "day 9"
  let s = 
        "7,1\n" ++
        "11,1\n" ++
        "11,7\n" ++
        "9,7\n" ++
        "9,5\n" ++
        "2,5\n" ++
        "2,3\n" ++
        "7,3"

  let posList = parseInput s
  print $ largestArea posList
  
  contents <- readFile "9.input"
  let posList2 = parseInput contents
  print $ largestArea posList

  -- test
  print $ largestArea2 posList2
