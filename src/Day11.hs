module Day11 where

import Data.Maybe (catMaybes)

data Point = Point Int Int deriving (Show, Eq, Ord)

flashes :: [[Char]] -> Int
flashes raw = flashes 100 (asInt raw)
  where
    flashes counter l
      | counter == 0 = 0
      | otherwise = numberOfFlashes + flashes (counter - 1) (increaseByOne . increaseByOne2 $ l)
        where
            numberOfFlashes = countFlashes . increaseByOne . increaseByOne2 $ l

asInt :: [[Char]] -> [[Int]]
asInt = map (map (\x -> read [x] :: Int))

countFlashes :: [[Int]] -> Int
countFlashes = sum . map (length . filter (== 0))

increaseByOne2 :: [[Int]] -> [[Int]]
increaseByOne2 = map (map (+ 1))

increaseByOne :: [[Int]] -> [[Int]]
increaseByOne lines = visit lines 0 0
  where
    visit lines rowIndex columnIndex
      | rowIndex == rows = lines
      | columnIndex == columns = visit lines (rowIndex + 1) 0
    visit lines rowIndex columnIndex
      | current > 9 = increaseByOne (flash (Point columnIndex rowIndex) (reset (Point columnIndex rowIndex) lines))
      | otherwise = visit lines rowIndex (columnIndex + 1)
      where
        current = lookup' (Point columnIndex rowIndex) lines

    columns = length . head $ lines
    rows = length lines

flash :: Point -> [[Int]] -> [[Int]]
flash point lines = flash' (neighbours point lines) lines
  where
    flash' [] m = m
    flash' ((Point columnIndex rowIndex, value) : xs) m
      | value == 0 = flash' xs m
      | otherwise = flash' xs (setElem (value + 1) (Point columnIndex rowIndex) m)

reset :: Point -> [[Int]] -> [[Int]]
reset = setElem 0

setElem :: Int -> Point -> [[Int]] -> [[Int]]
setElem value (Point columnIndex rowIndex) lines =
  take rowIndex lines ++ [updatedRow] ++ drop (rowIndex + 1) lines
  where
    updatedRow = take columnIndex rowToUpdate ++ [value] ++ drop (columnIndex + 1) rowToUpdate
    rowToUpdate = (!! max 0 rowIndex) lines

neighbours :: Point -> [[Int]] -> [(Point, Int)]
neighbours (Point columnIndex rowIndex) lines = catMaybes [left, right, upLeft, upRight, up, down, downLeft, downRight]
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
    upLeft
      | rowIndex - 1 < 0 || columnIndex - 1 < 0 = Nothing
      | otherwise = Just (point, lookup' point lines)
      where
        point = Point (columnIndex - 1) (rowIndex - 1)
    up
      | rowIndex - 1 < 0 = Nothing
      | otherwise = Just (point, lookup' point lines)
      where
        point = Point columnIndex (rowIndex - 1)
    upRight
      | rowIndex - 1 < 0 || columnIndex + 1 >= columns = Nothing
      | otherwise = Just (point, lookup' point lines)
      where
        point = Point (columnIndex + 1) (rowIndex - 1)
    downRight
      | rowIndex + 1 >= rows || columnIndex + 1 >= columns = Nothing
      | otherwise = Just (point, lookup' point lines)
      where
        point = Point (columnIndex + 1) (rowIndex + 1)
    down
      | rowIndex + 1 >= rows = Nothing
      | otherwise = Just (point, lookup' point lines)
      where
        point = Point columnIndex (rowIndex + 1)
    downLeft
      | rowIndex + 1 >= rows || columnIndex - 1 < 0 = Nothing
      | otherwise = Just (point, lookup' point lines)
      where
        point = Point (columnIndex - 1) (rowIndex + 1)

    columns = length . head $ lines
    rows = length lines

lookup' :: Point -> [[Int]] -> Int
lookup' (Point columnIndex rowIndex) lines = lines !! rowIndex !! columnIndex
