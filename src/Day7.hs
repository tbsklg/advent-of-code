module Day7 where

import Data.List.Split (splitOn)

leastFuel :: [[Char]] -> Int
leastFuel = minimumMovement moveByN . parseFuel

leastFuelPartTwo :: [[Char]] -> Int
leastFuelPartTwo = minimumMovement moveBySumOfN . parseFuel

minimumMovement :: (Int -> [Int] -> [Int]) -> [Int] -> Int
minimumMovement fuelStrategy l = minimum [sum . fuelStrategy x $ l | x <- [0 .. length l]]

parseFuel :: [[Char]] -> [Int]
parseFuel = map (\x -> read x :: Int) . splitOn "," . head

moveBySumOfN :: Int -> [Int] -> [Int]
moveBySumOfN n = map (\x -> gauss (abs (x - n)))
  where
    gauss x = (x * (x + 1)) `div` 2

moveByN :: Int -> [Int] -> [Int]
moveByN n = map (\x -> abs (x - n))
