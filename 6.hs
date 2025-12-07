{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read
import Data.Text.IO qualified as TIO
import Data.List (transpose)
import Data.List.Split (splitWhen)

-- =============================================================
--                      Part I
-- =============================================================
data Op = Add | Mul deriving (Show, Eq)

data Node = App Op [Int]
  deriving (Show, Eq)

evalExp :: Node -> Int
evalExp exp =
  case exp of
    App op nums ->
      case op of
        Add -> foldl (+) 0 nums
        Mul -> foldl (*) 1 nums

eval :: [Node] -> Int
eval exps = sum [evalExp e | e <- exps]

text2Int :: Text -> Int
text2Int t = case decimal t of
  Right (x, _) -> x
  Left e -> error $ show e
  
readOp :: Text -> Op
readOp "*" = Mul
readOp "+" = Add

-- parseInput :: Text -> [Node]
parseInput input =
  [App (readOp $ last e)
   [text2Int i | i <- (init e)]
  | e <- transpose ls]
  where lines = T.splitOn "\n" input
        ls = [filter (\x -> x /= "") (T.splitOn " " (T.strip line)) | line <- lines]


-- =============================================================
--                      Part II
-- =============================================================
parseInput2 :: Text -> [Node]
parseInput2 input = [parseProblem p | p <- problems]
  where input' = T.transpose (T.splitOn "\n" input)
        problems = splitWhen (\x -> T.strip x == "") input'

parseProblem :: [Text] -> Node
parseProblem ls = App op (n:nums)
  where
    fst = T.strip $ head ls
    op = readOp $ T.takeEnd 1 fst
    n = text2Int $ T.dropEnd 1 fst
    nums = [ text2Int $ T.strip n | n <- tail ls]

main :: IO ()
main = do
  print "day 6"
 
  contents <- TIO.readFile "6.input"
  let problem = parseInput contents
  print $ eval problem

  let problems2 = parseInput2 contents
  print $ eval problems2
