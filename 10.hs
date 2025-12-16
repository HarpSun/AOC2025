import Debug.Trace

import Text.ParserCombinators.Parsec
import Data.List (delete, concat, sortBy)

parseNum :: Parser Int
parseNum = do
  n <- many1 digit
  return (read n :: Int)
  
parseLights :: Parser String
parseLights = do
  res <- between (char '[') (char ']') (many (char '.' <|> char '#'))
  return res

parseButton :: Parser Button
parseButton = do
  many space
  res <- between (char '(') (char ')') (sepBy parseNum (char ','))
  many space
  return res

parseButtonList :: Parser [Button]
parseButtonList = do
  res <- manyTill parseButton (lookAhead (char '{'))
  return res

parseManual :: Parser ([Char], [Button])
parseManual = do
  lights <- parseLights
  buttons <- parseButtonList
  return (lights, buttons)
  
parseOrError :: Parser a -> String -> a
parseOrError p str =
  case parse p "" str of
    Left e -> error ("parser error" ++ (show e))
    Right a -> a
 
parseInput :: String -> [([Char], [Button])]
parseInput input = [parseOrError parseManual line | line <- lines input]

-- =============================================================
--                      Part I
-- =============================================================
type Button = [Int]

initLight :: [Char] -> [Char]
initLight light = take (length light) (repeat '.')
  
pressButton :: [Char] -> [Int] -> [Char]
pressButton lights button =
  [if i `elem` button then toggle l else l | (i, l) <- zip (iterate (+1) 0) lights]
  where toggle :: Char -> Char
        toggle '.' = '#'
        toggle '#' = '.'

type Memo = [((String, [Button]), [Button])]

configureMachine :: String -> String -> [Button] -> M.State Memo [Button]
configureMachine target lights buttons
  | lights == target = return []
  | otherwise = do
      let k = (lights, buttons)
      memo <- get
      case lookup k memo of
        Just a -> return a
        Nothing -> do
          let lights' = fmap (pressButton lights) buttons
          solutions <- mapM (uncurry $ configureMachine target)
                       [(l, delete b buttons) | (l, b) <- zip lights' buttons]
          let res = shortestSolution [(b:s) | (s, b) <- zip solutions buttons]
          modify' ((k, res):)
          return res

shortestSolution :: [[Button]] -> [Button]
shortestSolution [] = []
shortestSolution solutions = head sortedSolution
  where sortedSolution = sortBy (\x1 x2 -> compare (length x1) (length x2)) solutions

sumShortestSolution :: [([Char], [Button], [Int])] -> Int
sumShortestSolution machines = sum $ fmap length solutions
  where solutions = [ let (res, state) = runState (configureMachine light (initLight light) buttons) [] in res
                    | (light, buttons, _) <- machines]

-- =============================================================
--                      Part II
--   work for example input but too slow for real input
--                 ended up using z3...
-- =============================================================
initJoltage :: [Char] -> [Int]
initJoltage light = take (length light) (repeat 0)
  
pressButton2 :: [Int] -> [Int] -> [Int]
pressButton2 joltage button =
  [if i `elem` button then j + 1 else j | (i, j) <- zip (iterate (+1) 0) joltage]


type Memo2 = [([Int], Maybe [Button])]

configureMachine2 :: [Int] -> [Button] -> [Int] -> M.State Memo2 (Maybe [Button])
configureMachine2 target buttons joltage
  | joltage == target = return $ Just []
  | any id [ v1 > v2 | (v1, v2) <- zip joltage target] = return Nothing
  | otherwise = do
      let k = joltage
      memo <- get
      case lookup k memo of
        Just a -> return $ a
        Nothing -> do
          let joltage' = fmap (pressButton2 joltage) buttons
          solutions <- mapM (configureMachine2 target buttons) joltage'
          let solution = shortestSolution [let Just s' = s in (b:s') | (s, b) <- zip solutions buttons, s /= Nothing]
          let res = if solution == [] then Nothing else Just solution
          modify' ((k, res):)
          return res

sumShortestSolution2 :: [([Char], [Button], [Int])] -> Int
sumShortestSolution2 machines = sum $ fmap length [let Just s' = s in s' | s <- solutions, s /= Nothing]
  where solutions = [ let (res, state) = runState (configureMachine2 joltage buttons (initJoltage light)) [] in res
                    | (light, buttons, joltage) <- machines]
                    
main :: IO ()
main = do
  print "day 10"

  let s =
        "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n" ++
        "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n" ++
        "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
  
  let machines = parseInput s
  contents <- readFile "10.input"
  -- let machines2 = parseInput contents
  print $ sumShortestSolution machines
