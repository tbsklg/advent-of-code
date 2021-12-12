module Day9 where

import Data.List (sort, transpose)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (empty, insert, toList)
import Debug.Trace (traceShow)

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
    capture lines columnIndex rowIndex
      | columnIndex == rows = []
      | rowIndex == columns = capture lines (columnIndex + 1) 0
    capture lines columnIndex rowIndex
      | current < minimum neighbours' = (Point rowIndex columnIndex, current) : capture lines columnIndex (rowIndex + 1)
      | otherwise = capture lines columnIndex (rowIndex + 1)
      where
        neighbours' = map snd . neighbours (Point rowIndex columnIndex) $ lines
        current = valueOf columnIndex rowIndex
        valueOf r c = asInt (lines !! r !! c)

    columns = length . head $ lines
    rows = length lines

basins :: (Point, Int) -> [[Char]] -> [Point]
basins p rows = capture [p] empty
  where
    capture [] visited = toList visited
    capture ((point, value) : xs) l = capture (xs ++ neighbours') (insert point l)
      where
        neighbours' = filter (\(point, _) -> point `notElem` toList l) . filter (\(_, value) -> value /= 9) $ (neighbours point rows)

neighbours :: Point -> [[Char]] -> [(Point, Int)]
neighbours (Point rowIndex columnIndex) lines = catMaybes [left, right, up, down]
  where
    left
      | rowIndex - 1 < 0 = Nothing
      | otherwise = Just (Point (rowIndex - 1) columnIndex, valueOf columnIndex (rowIndex - 1))
    right
      | rowIndex + 1 >= columns = Nothing
      | otherwise = Just (Point (rowIndex + 1) columnIndex, valueOf columnIndex (rowIndex + 1))
    up
      | columnIndex - 1 < 0 = Nothing
      | otherwise = Just (Point rowIndex (columnIndex -1), valueOf (columnIndex - 1) rowIndex)
    down
      | columnIndex + 1 >= rows = Nothing
      | otherwise = Just (Point rowIndex (columnIndex + 1), valueOf (columnIndex + 1) rowIndex)

    columns = length . head $ lines
    rows = length lines
    valueOf r c = asInt (lines !! r !! c)

asInt :: Char -> Int
asInt x = read [x] :: Int
