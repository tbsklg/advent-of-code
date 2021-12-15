module Day13 where

import Data.List (partition)
import Data.List.Split (splitOn)
import Data.Set (fromList, toList)

data Instruction = Horizontal Int | Vertical Int deriving (Show, Eq)

solve :: [[Char]] -> Int
solve raw = length . foldPoints dots $ instruction
  where
    dots = fst . transform . extract $ raw
    instruction = head . snd . transform . extract $ raw

solvePartTwo :: [[Char]] -> [Char]
solvePartTwo raw = print' . foldl foldPoints dots $ instructions
  where
    dots = fst . transform . extract $ raw
    instructions = snd . transform . extract $ raw

print' :: [(Int, Int)] -> [Char]
print' dots = printRows 0 0
  where
    maxRows = 250
    maxCols = 250

    printRows row col
      | row == maxRows = ""
      | col == maxCols = "\n" ++ printRows (row + 1) 0
      | (row, col) `elem` dots = "#" ++ printRows row (col + 1)
      | (row, col) `notElem` dots = "." ++ printRows row (col + 1)
      | otherwise = printRows row col

extract :: [[Char]] -> ([[Char]], [[Char]])
extract raw = (dots, instructions)
  where
    dots = takeWhile (/= "") raw
    instructions = tail . dropWhile (/= "") $ raw

transform :: ([[Char]], [[Char]]) -> ([(Int, Int)], [Instruction])
transform raw = (dots, instructions)
  where
    dots = map (\x -> (xValue x, yValue x)) . fst $ raw
    xValue x = read (head . splitOn "," $ x) :: Int
    yValue y = read (last . splitOn "," $ y) :: Int

    instructions = map instruction . snd $ raw
    instruction x = direction (last . head . splitOn "=" $ x) (read (last . splitOn "=" $ x) :: Int)
      where
        direction 'x' y = Vertical y
        direction 'y' y = Horizontal y
        direction _ _ = error "Bad Instruction"

foldPoints :: [(Int, Int)] -> Instruction -> [(Int, Int)]
foldPoints dots (Vertical v) = toList . fromList $ movedPoints ++ otherPoints
  where
    movedPoints = map move relevantPoints
    relevantPoints = fst . partition isRelevant $ dots
    otherPoints = snd . partition isRelevant $ dots
    isRelevant (x, y) = x > v
    move (x, y) = (abs (x - 2 * v), y)
foldPoints dots (Horizontal h) = toList . fromList $ movedPoints ++ otherPoints
  where
    movedPoints = map move relevantPoints
    relevantPoints = fst . partition isRelevant $ dots
    otherPoints = snd . partition isRelevant $ dots
    isRelevant (x, y) = y > h
    move (x, y) = (x, abs (y - 2 * h))
