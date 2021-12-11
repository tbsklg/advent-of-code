module Day9 where

import Data.List (transpose)
import Debug.Trace (traceShow)

riskLevel :: [[Char]] -> Int
riskLevel = sum . map (+ 1) . riskLevels

riskLevels :: [[Char]] -> [Int]
riskLevels rows = lowest' rows 0 0
  where
    lowest' rows rowIndex columnIndex
      | rowIndex == nmbrOfRows = []
      | columnIndex == nmbrOfElements = lowest' rows (rowIndex + 1) 0
    lowest' rows rowIndex columnIndex
      | current < minimum neighbours = current : lowest' rows rowIndex (columnIndex + 1)
      | otherwise = lowest' rows rowIndex (columnIndex + 1)
      where
        neighbours = [left, right, up, down]
        current = valueOf rowIndex columnIndex
        valueOf r c = asInt (rows !! r !! c)
        left
          | columnIndex - 1 < 0 = valueOf rowIndex columnIndex + 1
          | otherwise = valueOf rowIndex (columnIndex - 1)
        right
          | columnIndex + 1 >= nmbrOfElements = valueOf rowIndex columnIndex + 1
          | otherwise = valueOf rowIndex (columnIndex + 1)
        up
          | rowIndex - 1 < 0 = valueOf rowIndex columnIndex + 1
          | otherwise = valueOf (rowIndex - 1) columnIndex
        down
          | rowIndex + 1 >= nmbrOfRows = valueOf rowIndex columnIndex + 1
          | otherwise = valueOf (rowIndex + 1) columnIndex

    nmbrOfElements = length . head $ rows
    nmbrOfRows = length rows

asInt :: Char -> Int
asInt x = read [x] :: Int