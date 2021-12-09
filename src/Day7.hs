module Day7 where

import Data.List.Split (splitOn)

leastFuel :: [[Char]] -> Int
leastFuel = minMovement . parseFuel
    where
        minMovement l =  minimum [sum . moveByN x $ l | x <-  [0 .. length l]]

parseFuel :: [[Char]] -> [Int]
parseFuel = map (\x -> read x :: Int) . splitOn "," . head

moveByN :: Int -> [Int] -> [Int]
moveByN n = map (\x -> abs (x - n))
