module Day1.SonarSweep (countIncreasingWindows) where

countIncreasingWindows :: (Ord a, Num p, Num a) => Int -> [a] -> p
countIncreasingWindows _ [] = 0
countIncreasingWindows n l
  | (sum . take n . tail $ l) > (sum . take n $ l) = (+) 1 . countIncreasingWindows n $ tail l
  | otherwise = countIncreasingWindows n $ tail l
