{-# LANGUAGE LambdaCase #-}

module Day4 (score, scorePartTwo) where

import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)

score :: [[Char]] -> Int
score raw = calculateScore . firstWinningBoard numbers $ boards
  where
    numbers = getNumbers . head $ raw
    boards = getBoards 5 . tail . tail $ raw

scorePartTwo :: [[Char]] -> Int
scorePartTwo raw = calculateScore . lastWinningBoard numbers $ boards
  where
    numbers = getNumbers . head $ raw
    boards = getBoards 5 . tail . tail $ raw

calculateScore :: (Int, [[Maybe Int]]) -> Int
calculateScore (number, board) = number * sumOfAllUnmarked board

firstWinningBoard :: [Int] -> [[[Maybe Int]]] -> (Int, [[Maybe Int]])
firstWinningBoard numbers boards = findFirstWinningBoard numbers boards []
  where
    findFirstWinningBoard [] [] _ = (0, [])
    findFirstWinningBoard [] (b : bs) _ = (0, [])
    findFirstWinningBoard (x : xs) [] s = findFirstWinningBoard xs s []
    findFirstWinningBoard (x : xs) (b : bs) s
      | hasBingo . markField x $ b = (x, markField x b)
      | otherwise = findFirstWinningBoard (x : xs) bs (markField x b : s)

lastWinningBoard :: [Int] -> [[[Maybe Int]]] -> (Int, [[Maybe Int]])
lastWinningBoard numbers board = last . findLastWinningBoard numbers board $ []
  where
    findLastWinningBoard [] [] _ = []
    findLastWinningBoard [] (b : bs) _ = []
    findLastWinningBoard (x : xs) [] s = findLastWinningBoard xs s []
    findLastWinningBoard (x : xs) (b : bs) s
      | hasBingo . markField x $ b = (x, markField x b) : findLastWinningBoard (x : xs) bs s
      | otherwise = findLastWinningBoard (x : xs) bs (markField x b : s)

getNumbers :: [Char] -> [Int]
getNumbers = map (\x -> read x :: Int) . splitOn ","

getBoards :: Int -> [[Char]] -> [[[Maybe Int]]]
getBoards _ [] = []
getBoards size raw = (map (map (\x -> Just (read x :: Int)) . words) . take size $ raw) : getBoards size (drop (size + 1) raw)

markField :: Int -> [[Maybe Int]] -> [[Maybe Int]]
markField field =
  map
    ( map
        ( \case
            Just a -> if a == field then Nothing else Just a
            Nothing -> Nothing
        )
    )

hasBingo :: [[Maybe Int]] -> Bool
hasBingo board = rowBingo board || columnBingo (rotateLeft board)
  where
    rowBingo [] = False
    rowBingo (x : xs) = ((==) 0 . length . filter (/= Nothing) $ x) || rowBingo xs
    columnBingo [] = False
    columnBingo (x : xs) = ((==) 0 . length . filter (/= Nothing) $ x) || columnBingo xs

rotateLeft :: [[Maybe Int]] -> [[Maybe Int]]
rotateLeft = reverse . transpose

sumOfAllUnmarked :: [[Maybe Int]] -> Int
sumOfAllUnmarked [] = 0
sumOfAllUnmarked (x : xs) = case fmap sum . sequence . filter (/= Nothing) $ x of
  Just a -> a + sumOfAllUnmarked xs
  Nothing -> 0 + sumOfAllUnmarked xs
