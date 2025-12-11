import Debug.Trace
import Data.List.Split (splitOn)
import Control.Monad.State.Strict

-- =============================================================
--                      Part I
-- =============================================================
type DeviceList = [(String, [String])]

data Node = Empty | Node { value :: String, outputs :: [Node] } deriving (Show)

parseInput :: String -> DeviceList
parseInput input = [
  let s = splitOn ":" line in (s !! 0, splitOn " " (tail $ s !! 1))
  | line <- lines input]
  where ls = lines input
  
makeConnection :: DeviceList -> String -> Node
makeConnection devices "out" = Empty
makeConnection devices value =
  case outputs of
    Nothing -> error ("can't find output from " ++ value)
    Just a -> Node value [ makeConnection devices out | out <- a]
  where outputs = lookup value devices

countPath :: Node -> Int
countPath Empty = 1
countPath node = sum [countPath out | out <- (outputs node)]


-- =============================================================
--                      Part II
-- =============================================================
type Memo = [((String, [String]), Int)]

countPath2 :: [String] -> Node -> State Memo Int
countPath2 ls Empty
  | "fft" `elem` ls && "dac" `elem` ls = return 1
  | otherwise = return 0
countPath2 ls node = do
  memo <- get
  case lookup (v, ls) memo of
    Just a -> return a
    Nothing -> do
      let ls' = if v `elem` ["fft", "dac"] then (v:ls) else ls
      res <- mapM (countPath2 ls') (outputs node)
      let total = sum res
      modify' (((v, ls), total):)
      return total
        
  where v = value node
  
main :: IO ()
main = do
  print "day 11"

  let input = unlines [
        "aaa: you hhh",
        "you: bbb ccc",
        "bbb: ddd eee",
        "ccc: ddd eee fff",
        "ddd: ggg",
        "eee: out",
        "fff: out",
        "ggg: out",
        "hhh: ccc fff iii",
        "iii: out"
        ]
  let devices = parseInput input
  let tree = makeConnection devices "you"
  print $ countPath tree

  contents <- readFile "11.input"
  let devices = parseInput contents
  let tree2 = makeConnection devices "you"
  print $ countPath tree2

  let input2 = unlines [
          "svr: aaa bbb",
          "aaa: fft",
          "fft: ccc",
          "bbb: tty",
          "tty: ccc",
          "ccc: ddd eee",
          "ddd: hub",
          "hub: fff",
          "eee: dac",
          "dac: fff",
          "fff: ggg hhh",
          "ggg: out",
          "hhh: out"
          ]
  let devices = parseInput input2
  let tree = makeConnection devices "svr"
  let (result, finalState) = runState (countPath2 [] tree) []
  print result

  let devices = parseInput contents
  let tree2 = makeConnection devices "svr"
  let (result, finalState) = runState (countPath2 [] tree2) []
  print result
