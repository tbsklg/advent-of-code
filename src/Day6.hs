module Day6 where

import Data.List.Split (splitOn)
import Data.Map (empty, fromList, fromListWith, insertWith, keys, toList, unionWith)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)

countFishes :: Int -> [[Char]] -> Int
countFishes days = count' days . fishes
  where
    count' _ [] = 0
    count' 0 l = traceShow (frequencies l) length l
    count' d l = count' (d - 1) . decreaseFish $ l

decreaseFish :: (Eq a, Num a) => [a] -> [a]
decreaseFish l
  | 0 `elem` l = decreaseByOne ++ replicate (length . filter (== 0) $ l) 8
  | otherwise = decreaseByOne
  where
    decreaseByOne = map (\x -> if x == 0 then 6 else x - 1) l

fishes :: [[Char]] -> [Int]
fishes = map (\x -> read x :: Int) . splitOn "," . head

decrease :: [(Int, Int)] -> [(Int, Int)]
decrease m = toList . decrease $ keys . fromList $ m
  where
    decrease [] = empty
    decrease (x : xs) = unionWith (+) (insertWith (+) (x - 1) by empty) (decrease xs)
      where
        by = fromMaybe 0 (lookup x m)

sumUp :: [(Int, Int)] -> Int
sumUp = sum . map snd

frequencies :: [Int] -> [(Int, Int)]
frequencies i = toList $ fromListWith (+) [(c, 1) | c <- i]