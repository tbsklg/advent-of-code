module Day15 where

import Data.Function (on)
import Data.List (sortBy, sortOn, transpose)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)

import qualified Data.Map as M
import qualified Data.Set as S

newtype ColumnIndex = ColumnIndex Int deriving (Show, Eq, Ord)

newtype RowIndex = RowIndex Int deriving (Show, Eq, Ord)

newtype Grid = Grid [[Char]] deriving (Show, Eq, Ord)

data Coordinates = Coordinates {columnIndex :: Int, rowIndex :: Int} deriving (Show, Eq, Ord)

data Point = Point {coordinates :: Coordinates, value :: Int} deriving (Show, Eq, Ord)

solve :: [[Char]] -> Int
solve raw = lowestRisk (Grid raw)

solvePartTwo :: [[Char]] -> Int
solvePartTwo raw = lowestRisk . expandGrid $ Grid raw

expandGrid :: Grid -> Grid
expandGrid (Grid grid) = Grid (concat . expandRow . map expandCell $ grid)

expandRow :: [[Char]] -> [[[Char]]]
expandRow = take 5 . iterate updateRisks
  where
    updateRisks [] = []
    updateRisks (x:xs) = concatMap (show . fixRisk) x : updateRisks xs
    fixRisk x = if asInt x == 9 then 1 else asInt x + 1

expandCell :: [Char] -> [Char]
expandCell = concat . take 5 . iterate updateRisks
  where
    updateRisks s = concatMap (show . fixRisk) s
    fixRisk x = if asInt x == 9 then 1 else asInt x + 1

-- todo: improve performance by indentifying equal costs

-- see https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
lowestRisk :: Grid -> Int
lowestRisk (Grid grid) = move initialQueue M.empty (S.fromList [initialCoordinates])
  where
    move priorityQueue costs visited
      | isTarget = currentDistance
      | currentCoordinates `elem` visited = move (tail priorityQueue) costs visited
      | otherwise = move nextQueue (M.insert currentCoordinates currentDistance costs) nextVisited
      where
        currentPoint = fst . head $ priorityQueue
        currentDistance = snd . head $ priorityQueue
        currentCoordinates = coordinates currentPoint
        isTarget = currentCoordinates == Coordinates {columnIndex = dim, rowIndex = dim}

        nextPoints = map (\point -> (point, currentDistance + value point)) . neighbours currentCoordinates $ Grid grid
        nextQueue = sortByDistance (tail priorityQueue ++ nextPoints)
        nextVisited = S.insert currentCoordinates visited

    initialQueue = sortByDistance . map (\point -> (point, value point)) . neighbours (coordinates (fst start)) $ Grid grid
    initialCoordinates = coordinates . fst $ start
    start = ((Point {coordinates = Coordinates {columnIndex = 0, rowIndex = 0}, value = 1}), 0)
    dim = length grid - 1

sortByDistance :: [(Point, Int)] -> [(Point, Int)]
sortByDistance = sortBy (compare `on` snd)

neighbours :: Coordinates -> Grid -> [Point]
neighbours Coordinates {columnIndex = c, rowIndex = r} (Grid grid) =
  catMaybes [up, down, right, left]
  where
    left
      | c - 1 < 0 = Nothing
      | otherwise = Just point
      where
        point = Point coordinates value
        value = lookup' coordinates (Grid grid)
        coordinates = Coordinates {columnIndex = c - 1, rowIndex = r}
    right
      | c + 1 >= maxColumns = Nothing
      | otherwise = Just point
      where
        point = Point coordinates value
        coordinates = Coordinates {columnIndex = c + 1, rowIndex = r}
        value = lookup' coordinates (Grid grid)
    up
      | r - 1 < 0 = Nothing
      | otherwise = Just point
      where
        point = Point coordinates value
        value = lookup' coordinates (Grid grid)
        coordinates = Coordinates {columnIndex = c, rowIndex = r - 1}
    down
      | r + 1 >= maxRows = Nothing
      | otherwise = Just point
      where
        point = Point coordinates value
        coordinates = Coordinates {columnIndex = c, rowIndex = r + 1}
        value = lookup' coordinates (Grid grid)

    maxColumns = length . head $ grid
    maxRows = length grid

lookup' :: Coordinates -> Grid -> Int
lookup' Coordinates {columnIndex = c, rowIndex = r} (Grid lines) = asInt (lines !! r !! c)

asInt :: Char -> Int
asInt x = read [x] :: Int
