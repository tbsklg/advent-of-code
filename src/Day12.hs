module Day12 where

import Data.Bifunctor (second)
import Data.Char (isLower)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

import qualified Data.Map as M
import qualified Data.Set as S

solve :: [[Char]] -> Int
solve = countPaths . groupNeighbours . extractPath

solvePartTwo :: [[Char]] -> Int
solvePartTwo = countPathsPartTwo . groupNeighbours . extractPath

extractPath :: [[Char]] -> [([Char], [Char])]
extractPath = map (\x -> (head . splitByMinus $ x, last . splitByMinus $ x))
  where
    splitByMinus x = splitOn "-" x

groupNeighbours :: [([Char], [Char])] -> [([Char], [[Char]])]
groupNeighbours m =
  filter (\(key, _) -> key /= "end")
    . map (second (filter (/= "start")))
    . M.toList
    . M.unionWith (++) groupByKey
    $ groupByValue
  where
    groupByKey = M.fromListWith (++) [(k, [v]) | (k, v) <- m]
    groupByValue = M.fromListWith (++) [(v, [k]) | (k, v) <- m]

countPaths :: Num a => [([Char], [[Char]])] -> a
countPaths m = traverse' (neighbours "start" m) S.empty
  where
    traverse' [] _ = 0
    traverse' (x : xs) visited
      | x == "end" = 1 + traverse' xs visited
      | S.member x visited = traverse' xs visited
      | isLowerCase x = traverse' (neighbours x m) (S.insert x visited) + traverse' xs visited
      | otherwise = traverse' (neighbours x m) visited + traverse' xs visited

countPathsPartTwo :: Num a => [([Char], [[Char]])] -> a
countPathsPartTwo m = traverse' "start" S.empty M.empty
  where
    traverse' curr visited counts
      | (> 1) . length . filter (== 2) $ M.elems counts = 0
      | S.member curr visited = 0
      | (Just 2 ==) $ M.lookup curr counts = 0
      | curr == "end" = 1
      | otherwise =
        let newVisited = if curr == "start" then S.insert curr visited else visited
            newCounts = if small then M.insertWith (+) curr 1 counts else counts
            small = isLowerCase curr && curr /= "start" && curr /= "end"
         in sum . map (\n -> traverse' n newVisited newCounts) $ neighbours curr m

isLowerCase :: [Char] -> Bool
isLowerCase = foldl (\y x -> y && isLower x) True

neighbours :: [Char] -> [([Char], [[Char]])] -> [[Char]]
neighbours key m = fromMaybe [] (lookup key m)
