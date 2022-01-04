module Day20 where
import Data.Char

data Coordinate = Coordinate Int Int deriving (Show, Eq, Ord)

extract :: [[Char]] -> (String, [String])
extract raw = (iea, img)
  where
    iea = head raw
    img = tail . tail $ raw

determinePixels :: Coordinate -> [[Char]] -> [[Char]]
determinePixels (Coordinate x y) img
  | x > rows || y > columns = ["...", "...", "..."]
  | otherwise = [upper, middle, lower]
  where
    current = lookup' (Coordinate x y) img
    left
      | y - 1 < 0 = '.'
      | otherwise = lookup' (Coordinate x (y -1)) img
    right
      | y + 1 >= columns = '.'
      | otherwise = lookup' (Coordinate x (y + 1)) img
    upLeft
      | y - 1 < 0 || x - 1 < 0 = '.'
      | otherwise = lookup' (Coordinate (x -1) (y -1)) img
    up
      | x - 1 < 0 = '.'
      | otherwise = lookup' (Coordinate (x -1) y) img
    upRight
      | x - 1 < 0 || y + 1 >= columns = '.'
      | otherwise = lookup' (Coordinate (x - 1) (y + 1)) img
    downRight
      | x + 1 >= rows || y + 1 >= columns = '.'
      | otherwise = lookup' (Coordinate (x + 1) (y + 1)) img
    down
      | x + 1 >= rows = '.'
      | otherwise = lookup' (Coordinate (x + 1) y) img
    downLeft
      | x + 1 >= rows || y - 1 < 0 = '.'
      | otherwise = lookup' (Coordinate (x + 1) (y - 1)) img

    upper = [upLeft] ++ [up] ++ [upRight]
    middle = [left] ++ [current] ++ [right]
    lower = [downLeft] ++ [down] ++ [downRight]

    columns = length . head $ img
    rows = length img

ieaIndex :: [[Char]] -> Int
ieaIndex = toDec . map (\x -> if x == '.' then '0' else '1') . concat

toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

lookup' :: Coordinate -> [String] -> Char
lookup' (Coordinate x y) img = img !! x !! y