module Day6 where

import Data.List.Split (splitOn)
import Data.Map (empty, fromList, fromListWith, insertWith, keys, toList, unionWith)
import Data.Maybe (fromMaybe)

countFishes :: Integer -> [[Char]] -> Integer
countFishes days raw = count' days (frequencies . fishes $ raw)
  where
    count' _ [] = 0
    count' 0 l = sumUp l
    count' d l = count' (d - 1) . populate $ l

fishes :: [[Char]] -> [Integer]
fishes = map (\x -> read x :: Integer) . splitOn "," . head

populate :: [(Integer, Integer)] -> [(Integer, Integer)]
populate m = toList . populate $ keys . fromList $ m
  where
    populate [] = empty
    populate (x : xs) = unionWith (+) next (populate xs)
      where
        next
          | x == 0 = unionWith (+) (insertWith (+) 8 by empty) (insertWith (+) 6 by empty)
          | otherwise = insertWith (+) (x -1) by empty
        by = fromMaybe 0 (lookup x m)

sumUp :: [(Integer, Integer)] -> Integer
sumUp = sum . map snd

frequencies :: [Integer] -> [(Integer, Integer)]
frequencies i = toList $ fromListWith (+) [(c, 1) | c <- i]
