module Day9 where

import Data.List (sort, transpose)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (empty, insert, toList)

data Point = Point Int Int deriving (Show, Eq, Ord)

riskLevel :: [[Char]] -> Int
riskLevel =
  sum
    . map ((+ 1) . snd)
    . riskLevels

basin :: [[Char]] -> Int
basin rows =
  product
    . take 3
    . reverse
    . sort
    . map (length . (`basins` rows))
    $ riskLevels rows

riskLevels :: [[Char]] -> [(Point, Int)]
riskLevels lines = capture lines 0 0
  where
    capture lines rowIndex columnIndex
      | rowIndex == rows = []
      | columnIndex == columns = capture lines (rowIndex + 1) 0
    capture lines rowIndex columnIndex
      | current < minimum currentNeighbours = (Point columnIndex rowIndex, current) : capture lines rowIndex (columnIndex + 1)
      | otherwise = capture lines rowIndex (columnIndex + 1)
      where
        currentNeighbours = map snd . neighbours (Point columnIndex rowIndex) $ lines
        current = lookup' (Point columnIndex rowIndex) lines

    columns = length . head $ lines
    rows = length lines

basins :: (Point, Int) -> [[Char]] -> [Point]
basins p rows = capture [p] empty
  where
    capture [] visited = toList visited
    capture ((point, value) : xs) visited = capture (xs ++ currentNeighbours) (insert point visited)
      where
        currentNeighbours =
          filter notYetVisited
            . filter notNine
            $ neighbours point rows

        notYetVisited (point, _) = point `notElem` toList visited
        notNine (_, value) = value /= 9

neighbours :: Point -> [[Char]] -> [(Point, Int)]
neighbours (Point columnIndex rowIndex) lines = catMaybes [left, right, up, down]
  where
    left
      | columnIndex - 1 < 0 = Nothing
      | otherwise = Just (point, lookup' point lines)
      where
        point = Point (columnIndex - 1) rowIndex
    right
      | columnIndex + 1 >= columns = Nothing
      | otherwise = Just (point, lookup' point lines)
      where
        point = Point (columnIndex + 1) rowIndex
    up
      | rowIndex - 1 < 0 = Nothing
      | otherwise = Just (point, lookup' point lines)
      where
        point = Point columnIndex (rowIndex - 1)
    down
      | rowIndex + 1 >= rows = Nothing
      | otherwise = Just (point, lookup' point lines)
      where
        point = Point columnIndex (rowIndex + 1)

    columns = length . head $ lines
    rows = length lines

lookup' :: Point -> [[Char]] -> Int
lookup' (Point columnIndex rowIndex) lines = asInt (lines !! rowIndex !! columnIndex)

asInt :: Char -> Int
asInt x = read [x] :: Int
