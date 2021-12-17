module Day15 where

import Data.Maybe (catMaybes)

data Direction = U | D | L | R deriving (Show, Eq)

newtype ColumnIndex = ColumnIndex Int deriving (Show, Eq)

newtype RowIndex = RowIndex Int deriving (Show, Eq)

data Coordinates = Coordinates ColumnIndex RowIndex deriving (Show, Eq)

data Point = Point Coordinates Int deriving (Show, Eq)

neighbours :: Coordinates -> [[Char]] -> [(Point, Direction)]
neighbours (Coordinates (ColumnIndex column) (RowIndex row)) grid = catMaybes [left, right, up, down]
  where
    left
      | column - 1 < 0 = Nothing
      | otherwise = Just (point, L)
      where
        point = Point coordinates value
        value = lookup' coordinates grid
        coordinates = Coordinates (ColumnIndex (column - 1)) (RowIndex row)
    right
      | column + 1 >= maxColumns = Nothing
      | otherwise = Just (point, R)
      where
        point = Point coordinates value
        value = lookup' coordinates grid
        coordinates = Coordinates (ColumnIndex (column + 1)) (RowIndex row)
    up
      | row - 1 < 0 = Nothing
      | otherwise = Just (point, U)
      where
        point = Point coordinates value
        value = lookup' coordinates grid
        coordinates = Coordinates (ColumnIndex column) (RowIndex (row - 1))
    down
      | row + 1 >= maxRows = Nothing
      | otherwise = Just (point, D)
      where
        point = Point coordinates value
        value = lookup' coordinates grid
        coordinates = Coordinates (ColumnIndex column) (RowIndex (row + 1))

    maxColumns = length . head $ grid
    maxRows = length grid

lookup' :: Coordinates -> [[Char]] -> Int
lookup' (Coordinates (ColumnIndex column) (RowIndex row)) lines = asInt (lines !! column !! row)

asInt :: Char -> Int
asInt x = read [x] :: Int
