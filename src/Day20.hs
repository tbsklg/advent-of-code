module Day20 where

import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Day16 (binToDec, solvePartTwo)

data Coordinate = Coordinate Int Int deriving (Show, Eq, Ord)

data Direction = U | L | R | D deriving (Show, Eq, Ord)

solve :: [[Char]] -> Int
solve = litPixels . executeIea 2 . extract

solvePartTwo :: [[Char]] -> Int
solvePartTwo = litPixels . executeIea 50 . extract

print' :: [[Char]] -> [Char]
print' = concatMap (\x -> x ++ ['\n'])

extract :: [[Char]] -> (String, [String])
extract raw = (iea, img)
  where
    iea = head raw
    img = tail . tail $ raw

litPixels :: [[Char]] -> Int
litPixels = length . filter (== '#') . concat

executeIea :: Int -> (String, [[Char]]) -> [[Char]]
executeIea times (iea, img) = executeIea' (iea, expandedImg) times
  where
    executeIea' (iea, img) 0 = img
    executeIea' (iea, img) cnt = executeIea' (output (iea, img) outerPixel') (cnt - 1)
      where
        outerPixel' = if even cnt then '.' else '#'

    expandedImg = last . take 100 . iterate expandImg $ img

toggleIndex :: Int -> Int
toggleIndex 0 = binToDec "111111111"
toggleIndex _ = 0

outerPixel :: Int -> String -> Char
outerPixel index iea = iea !! index

output :: (String, [[Char]]) -> Char -> (String, [[Char]])
output (iea, img) outer = (iea, output (zip [0 ..] img))
  where
    output [] = []
    output ((rowIndex, columns) : xs) =
      zipWith (\columnIndex _ -> outputPixel (Coordinate rowIndex columnIndex) (iea, img) outer) [0 ..] columns : output xs

outputPixel :: Coordinate -> (String, [String]) -> Char -> Char
outputPixel coordinate (iea, img) outer = ieaValue'
  where
    relevantPixels = determinePixels coordinate img outer
    ieaIndex' = ieaIndex relevantPixels
    ieaValue' = ieaValue iea ieaIndex'

expandImg :: [[Char]] -> [[Char]]
expandImg img = upper ++ middle ++ lower
  where
    middle = map (\x -> ['.'] ++ x ++ ['.']) img
    upper = [replicate (length . head $ middle) '.']
    lower = [replicate (length . head $ middle) '.']

determinePixels :: Coordinate -> [[Char]] -> Char -> [[Char]]
determinePixels (Coordinate x y) img pixel
  | x > rows || y > columns = ["...", "...", "..."]
  | otherwise = [upper, middle, lower]
  where
    current = lookup' (Coordinate x y) img
    left
      | y - 1 < 0 = pixel
      | otherwise = lookup' (Coordinate x (y -1)) img
    right
      | y + 1 >= columns = pixel
      | otherwise = lookup' (Coordinate x (y + 1)) img
    upLeft
      | y - 1 < 0 || x - 1 < 0 = pixel
      | otherwise = lookup' (Coordinate (x -1) (y -1)) img
    up
      | x - 1 < 0 = pixel
      | otherwise = lookup' (Coordinate (x -1) y) img
    upRight
      | x - 1 < 0 || y + 1 >= columns = pixel
      | otherwise = lookup' (Coordinate (x - 1) (y + 1)) img
    downRight
      | x + 1 >= rows || y + 1 >= columns = pixel
      | otherwise = lookup' (Coordinate (x + 1) (y + 1)) img
    down
      | x + 1 >= rows = pixel
      | otherwise = lookup' (Coordinate (x + 1) y) img
    downLeft
      | x + 1 >= rows || y - 1 < 0 = pixel
      | otherwise = lookup' (Coordinate (x + 1) (y - 1)) img

    upper = [upLeft] ++ [up] ++ [upRight]
    middle = [left] ++ [current] ++ [right]
    lower = [downLeft] ++ [down] ++ [downRight]

    columns = length . head $ img
    rows = length img

ieaValue :: [Char] -> Int -> Char
ieaValue iea index = iea !! index

ieaIndex :: [[Char]] -> Int
ieaIndex = toDec . map (\x -> if x == '.' then '0' else '1') . concat

toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

lookup' :: Coordinate -> [String] -> Char
lookup' (Coordinate x y) img = img !! x !! y
