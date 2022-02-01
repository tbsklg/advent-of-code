module Day25 where

import Data.List (transpose)

solve :: Num t => [[Char]] -> t
solve raw = simulate raw 1
  where
    simulate raw cnt
      | raw == performStep raw = cnt
      | otherwise = simulate (performStep raw) (cnt + 1)

performStep :: [[Char]] -> [[Char]]
performStep rows = performSouth
  where
    performEast = map east rows
    performSouth = south performEast

east :: [Char] -> [Char]
east row
  | head row == '.' && last row == '>' = '>' : (init . tail . next row $ '>') ++ ['.']
  | otherwise = next row '>'

south :: [[Char]] -> [[Char]]
south = transpose . south . transpose
  where
    south [] = []
    south (x : xs)
      | head x == '.' && last x == 'v' = ('v' : (init . tail . next x $ 'v') ++ ['.']) : south xs
      | otherwise = next x 'v' : south xs

next :: [Char] -> Char -> [Char]
next [] _ = []
next [x] c = [x]
next (x : y : xs) c
  | x == c && y == '.' = '.' : c : next xs c
  | otherwise = x : next (y : xs) c
