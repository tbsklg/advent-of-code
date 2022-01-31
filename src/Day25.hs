module Day25 where

import Day21 (nextSpace)

step :: [Char] -> [Char]
step row
  | head row == '.' && last row == '>' = '>' : (init . tail . next $ row) ++ ['.']
  | otherwise = next row
  where
    next [] = []
    next [x] = [x]
    next (x : y : xs)
      | x == '>' && y == '.' = '.' : '>' : next xs
      | otherwise = x : next (y : xs)