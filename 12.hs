import Debug.Trace
import Data.Array
import Data.Array.IArray ((!?))
import Data.List (intercalate, unfoldr)
import Data.List.Split (splitOn, splitWhen)
import Control.Monad.State.Lazy
import Control.Monad (forM)

  
-- =============================================================
--                      Part I
-- =============================================================
type Pos = (Int, Int)
type Region = Array Pos Char
type Present = Array Pos Char

parsePresent :: [String] -> Present
parsePresent input = listArray ((0, 0), (rows, cols)) (intercalate "" ls)
  where ls = tail input
        rows = length ls - 1
        cols = (length $ head ls) - 1
        
parseRegion :: String -> ((Int, Int), [Int])
parseRegion input = (size, indices)
  where ls = splitOn " " input
        size = let p = splitOn "x" $ init $ head ls
               in (read $ p !! 0 :: Int, read $ p !! 1 :: Int)
        indices = [read i :: Int | i <- tail ls]
  
parseInput :: String -> ([Present], [((Int, Int), [Int])])
parseInput input = (presents, regions)
  where ls = splitWhen (\x -> x == "") $ lines input
        regionsInput = last ls
        presentInput = init ls
        presents = [parsePresent p | p <- presentInput]
        regions = [parseRegion r | r <- regionsInput]
        
initRegion :: Int -> Int -> Region
initRegion w h = listArray ((0, 0), (h - 1, w - 1)) (iterate id '.')

splitList :: Int -> [a] -> [[a]]
splitList n [] = []
splitList n ls = (take n ls) : (splitList n (drop n ls))

showRegion :: Region -> String
showRegion map = intercalate "\n" (splitList (row + 1) chars) ++ "\n"
  where ((0, 0), (row, col)) = bounds map
        chars = elems map

serializeArray :: Array Pos Char -> String
serializeArray map = elems map
        
placePresent :: Region -> Present -> Pos -> Maybe Region
placePresent region present startPos = case traverse (region !?) present' of
  Nothing -> Nothing
  Just vals -> if any (\x -> x == '#') vals then Nothing else Just $ region // [(p, '#') | p <- present']
  where present' = transPos (presentPos present) startPos
        transPos :: [Pos] -> Pos -> [Pos]
        transPos posList (x0, y0) = [ (x + x0, y + y0) | (x, y) <- posList]

        presentPos :: Present -> [Pos]
        presentPos present = [i | (i, e) <- assocs present, e == '#']

presentForms :: Present -> [Present]
presentForms present = rotations ++ [flip r | r <- rotations]
  where rotations = take 4 $ unfoldr (\p -> Just (p, rotate p)) present
        rotate :: Present -> Present
        rotate present = listArray ((0, 0), (rows, cols)) (iterate id '.') // update
          where ((0, 0), (rows, cols)) = bounds present
                update = [((x, cols - y), e) | ((y, x), e) <- assocs present, e == '#']
                
        flip :: Present -> Present
        flip present = listArray ((0, 0), (rows, cols)) (iterate id '.') // update
          where ((0, 0), (rows, cols)) = bounds present
                update = [((y, cols - x), e) | ((y, x), e) <- assocs present, e == '#']

type Memo = [(String, Bool)]
  
fitRegion :: Region -> [Present] -> State Memo Bool
fitRegion region [] = return True
fitRegion region presents = do
  memo <- get
  let memo = []
  case lookup k memo of
    Just a -> return a
    Nothing -> do
      let res = [placePresent region p pos | pos <- posList, p <- presentForms present]
      let regions' = [let Just region' = r in region' | r <- res, r /= Nothing]
      fit <- any id <$> mapM (flip fitRegion $ left) [let Just region' = r in region' | r <- res, r /= Nothing]
      -- let fit = any id fitRes
      modify ((k, fit):)
      return fit
  
  where posList = indices region
        left = tail presents
        present = head presents
        k = foldl (++) (serializeArray region) [serializeArray p | p <- presents]
  
countFitRegion :: ([Present], [((Int, Int), [Int])]) -> Int
countFitRegion (presents, regions) = length $ filter id fitRes
  where fitRes = [let r = runState (fitRegion (initRegion w h) $ genPresents presents indices) []
                  in (fst r)
                 | ((w, h), indices) <- regions]
        genPresents :: [Present] -> [Int] -> [Present]
        genPresents presents indices = foldl (++) [] [
          take n (iterate id $ presents !! i)
          | (n, i) <- zip indices (iterate (+1) 0)]


-- =============================================================
--                      Part I workround
-- =============================================================
neverFit :: [Present] -> (Int, Int) -> Bool
neverFit presents (w, h) = pArea > area
  where area = w * h
        pArea = sum [calArea p | p <- presents]
        calArea :: Present -> Int
        calArea present = length $ filter (\x -> x == '#') $ elems present

mustFit :: [Present] -> (Int, Int) -> Bool
mustFit presents (w, h) = area >= pArea
  where area = w * h
        pArea = (length presents) * 9
        
-- merry christmas!
countFitRegion2 :: ([Present], [((Int, Int), [Int])]) -> (Int, Int, Int)
countFitRegion2 (presents, regions) = (length regions, neverFits, mustFits)
  where
    neverFits = length $ filter id [ neverFit (genPresents presents indices) (w, h)
                                   | ((w, h), indices) <- regions]
    mustFits = length $ filter id [ mustFit (genPresents presents indices) (w, h)
                                   | ((w, h), indices) <- regions]
    genPresents :: [Present] -> [Int] -> [Present]
    genPresents presents indices = foldl (++) [] [
      take n (iterate id $ presents !! i)
      | (n, i) <- zip indices (iterate (+1) 0)]

main :: IO ()
main = do
  print "day 12"

  contents <- readFile "12.input"
  let puzzle = parseInput contents
  print $ countFitRegion2 puzzle
