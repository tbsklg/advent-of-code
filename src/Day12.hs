module Day12 where

import Data.Bifunctor (second)
import Data.Char (isLower)
import Data.List.Split (splitOn)
import Data.Map (fromListWith, toList, unionWith)
import Data.Maybe (fromMaybe)
import Data.Set (delete, empty, insert, member)
import Debug.Trace (traceShow)

solve :: [[Char]] -> Int
solve = countPaths . groupNeighbours . extractPath

extractPath :: [[Char]] -> [([Char], [Char])]
extractPath = map (\x -> (head . splitByMinus $ x, last . splitByMinus $ x))
  where
    splitByMinus x = splitOn "-" x

groupNeighbours :: [([Char], [Char])] -> [([Char], [[Char]])]
groupNeighbours m =
  filter (\(key, _) -> key /= "end")
    . map (second (filter (/= "start")))
    . toList
    . unionWith (++) groupByKey
    $ groupByValue
  where
    groupByKey = fromListWith (++) [(k, [v]) | (k, v) <- m]
    groupByValue = fromListWith (++) [(v, [k]) | (k, v) <- m]

countPaths :: Num a => [([Char], [[Char]])] -> a
countPaths m = traverse' (neighbours "start" m) empty
  where
    traverse' [] _ = 0
    traverse' (x : xs) visited
      | x == "end" = 1 + traverse' xs visited
      | x `member` visited = traverse' xs visited
      | isLowerCase x = traverse' (neighbours x m) (insert x visited) + traverse' xs visited
      | otherwise = traverse' (neighbours x m) visited + traverse' xs visited

isLowerCase :: [Char] -> Bool
isLowerCase = foldl (\y x -> y && isLower x) True

neighbours :: [Char] -> [([Char], [[Char]])] -> [[Char]]
neighbours key m = fromMaybe [] (lookup key m)