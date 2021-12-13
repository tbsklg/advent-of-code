module Day11 where

import Data.Maybe (catMaybes)

data Octopus = Octopus Int Int deriving (Show, Eq, Ord)

flashes :: [[Char]] -> Int
flashes raw = flashes 100 (asInt raw)
  where
    flashes counter l
      | counter == 0 = 0
      | otherwise = numberOfFlashes + flashes (counter - 1) (performFlash . incrementAll $ l)
      where
        numberOfFlashes = countFlashes . performFlash . incrementAll $ l

flashesPartTwo :: [[Char]] -> Int
flashesPartTwo raw = flashes 1 (asInt raw)
  where
    flashes counter l
      | numberOfFlashes == numberOfOctopuses = counter
      | otherwise = flashes (counter + 1) . performFlash . incrementAll $ l
      where
        numberOfOctopuses = rows * columns
        numberOfFlashes = countFlashes . performFlash . incrementAll $ l
        rows = length raw
        columns = length . head $ raw

asInt :: [[Char]] -> [[Int]]
asInt = map (map (\x -> read [x] :: Int))

countFlashes :: [[Int]] -> Int
countFlashes = sum . map (length . filter (== 0))

incrementAll :: [[Int]] -> [[Int]]
incrementAll = map (map (+ 1))

performFlash :: [[Int]] -> [[Int]]
performFlash lines = visit lines 0 0
  where
    visit lines rowIndex columnIndex
      | rowIndex == rows = lines
      | columnIndex == columns = visit lines (rowIndex + 1) 0
    visit lines rowIndex columnIndex
      | currentValue > 9 = performFlash (flashFor (Octopus columnIndex rowIndex) (reset (Octopus columnIndex rowIndex) lines))
      | otherwise = visit lines rowIndex (columnIndex + 1)
      where
        currentValue = lookup' (Octopus columnIndex rowIndex) lines

    columns = length . head $ lines
    rows = length lines

flashFor :: Octopus -> [[Int]] -> [[Int]]
flashFor octopus lines = flashFor' (neighbours octopus lines) lines
  where
    flashFor' [] m = m
    flashFor' ((Octopus columnIndex rowIndex, value) : xs) m
      | value == 0 = flashFor' xs m
      | otherwise = flashFor' xs (setElem (value + 1) (Octopus columnIndex rowIndex) m)

reset :: Octopus -> [[Int]] -> [[Int]]
reset = setElem 0

setElem :: Int -> Octopus -> [[Int]] -> [[Int]]
setElem value (Octopus columnIndex rowIndex) lines =
  take rowIndex lines ++ [updatedRow] ++ drop (rowIndex + 1) lines
  where
    updatedRow = take columnIndex rowToUpdate ++ [value] ++ drop (columnIndex + 1) rowToUpdate
    rowToUpdate = (!! max 0 rowIndex) lines

neighbours :: Octopus -> [[Int]] -> [(Octopus, Int)]
neighbours (Octopus columnIndex rowIndex) lines = catMaybes [left, right, upLeft, upRight, up, down, downLeft, downRight]
  where
    left
      | columnIndex - 1 < 0 = Nothing
      | otherwise = Just (octopus, lookup' octopus lines)
      where
        octopus = Octopus (columnIndex - 1) rowIndex
    right
      | columnIndex + 1 >= columns = Nothing
      | otherwise = Just (octopus, lookup' octopus lines)
      where
        octopus = Octopus (columnIndex + 1) rowIndex
    upLeft
      | rowIndex - 1 < 0 || columnIndex - 1 < 0 = Nothing
      | otherwise = Just (octopus, lookup' octopus lines)
      where
        octopus = Octopus (columnIndex - 1) (rowIndex - 1)
    up
      | rowIndex - 1 < 0 = Nothing
      | otherwise = Just (octopus, lookup' octopus lines)
      where
        octopus = Octopus columnIndex (rowIndex - 1)
    upRight
      | rowIndex - 1 < 0 || columnIndex + 1 >= columns = Nothing
      | otherwise = Just (octopus, lookup' octopus lines)
      where
        octopus = Octopus (columnIndex + 1) (rowIndex - 1)
    downRight
      | rowIndex + 1 >= rows || columnIndex + 1 >= columns = Nothing
      | otherwise = Just (octopus, lookup' octopus lines)
      where
        octopus = Octopus (columnIndex + 1) (rowIndex + 1)
    down
      | rowIndex + 1 >= rows = Nothing
      | otherwise = Just (octopus, lookup' octopus lines)
      where
        octopus = Octopus columnIndex (rowIndex + 1)
    downLeft
      | rowIndex + 1 >= rows || columnIndex - 1 < 0 = Nothing
      | otherwise = Just (octopus, lookup' octopus lines)
      where
        octopus = Octopus (columnIndex - 1) (rowIndex + 1)

    columns = length . head $ lines
    rows = length lines

lookup' :: Octopus -> [[Int]] -> Int
lookup' (Octopus columnIndex rowIndex) lines = lines !! rowIndex !! columnIndex
