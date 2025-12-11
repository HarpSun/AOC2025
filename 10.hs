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

configureMachine :: [Char] -> [Button] -> [Char] -> [Button]
configureMachine lights buttons target
  | lights == target = []
  | otherwise =
      let lights' = fmap (pressButton lights) buttons
          solutions = [b : (configureMachine l (delete b buttons) target) | (l, b) <- zip lights' buttons]
      in shortestSolution solutions

shortestSolution :: [[Button]] -> [Button]
shortestSolution [] = []
shortestSolution solutions = head sortedSolution
  where sortedSolution = sortBy (\x1 x2 -> compare (length x1) (length x2)) solutions

sumShortestSolution :: [([Char], [Button])] -> Int
sumShortestSolution machines = 
  sum $ fmap length [configureMachine (initLight light) buttons light | (light, buttons) <- machines]

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
