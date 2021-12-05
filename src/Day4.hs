{-# LANGUAGE LambdaCase #-}
module Day4 (score) where

import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

score :: [[Char]] -> Int
score raw = calculateScore numbers boards
  where
    calculateScore n b = checkBoards n b []
      where
        checkBoards [] [] _ = 0
        checkBoards [] (b : bs) _ = 0
        checkBoards (x : xs) [] s = checkBoards xs s []
        checkBoards (x : xs) (b : bs) s
          | hasBingo . markField x $ b = x * (sumOfAllUnmarked . markField x $ b)
          | otherwise = checkBoards (x : xs) bs (markField x b : s)
    numbers = getNumbers . head $ raw
    boards = getBoards 5 . tail . tail $ raw

getNumbers :: [Char] -> [Int]
getNumbers = map (\x -> read x :: Int) . splitOn ","

getBoards :: Int -> [[Char]] -> [[[Maybe Int]]]
getBoards _ [] = []
getBoards size raw = (map (map (\x -> Just (read x :: Int)) . words) . take size $ raw) : getBoards size (drop (size + 1) raw)

-- abort when field is marked
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
