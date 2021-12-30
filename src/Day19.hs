module Day19 where

import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Split
import Data.Set (fromList, toList)
import Debug.Trace

data Vector a = Vector a a a deriving (Show, Eq, Ord)

data Scanner a = Scanner Int (Vector a) [Vector a] deriving (Show, Eq, Ord)

-- if offset between 12 Vectors for scanners with orientations/flips is equal, then we have the overlapping scanner

extract :: [[Char]] -> [Scanner Int]
extract [] = []
extract (x : xs)
  | "---" `isPrefixOf` x =
    Scanner id position beacons : extract next
  | otherwise = extract xs
  where
    id = read (takeWhile isDigit . dropWhile (not . isDigit) $ x) :: Int
    position = Vector 0 0 0
    beacons = map coordinates . takeWhile (/= "") $ xs

    next = dropWhile (/= "") xs

coordinates :: [Char] -> Vector Int
coordinates raw = traceShow raw Vector x y z
  where
    x = head values
    y = head . tail $ values
    z = last values
    values = map (\x -> read x :: Int) splitByComma
    splitByComma = splitOn "," raw

rotZ90 :: (Num a) => Vector a -> Vector a
rotZ90 (Vector x y z) = Vector y (x * (-1)) z

rotX90 :: (Num a) => Vector a -> Vector a
rotX90 (Vector x y z) = Vector x z (y * (-1))

rotY90 :: (Num a) => Vector a -> Vector a
rotY90 (Vector x y z) = Vector z y (x * (-1))

flipX :: (Num a) => Vector a -> Vector a
flipX (Vector x y z) = Vector (x * (-1)) y z

flipY :: (Num a) => Vector a -> Vector a
flipY (Vector x y z) = Vector x (y * (-1)) z

flipZ :: (Num a) => Vector a -> Vector a
flipZ (Vector x y z) = Vector x y (z * (-1))

rotations :: (Num a, Ord a) => [Vector a] -> [Vector a]
rotations =
  toList
    . fromList
    . concatMap
      ( \x ->
          rotateZ (flipZ x)
            ++ rotateY (flipY x)
            ++ rotateX (flipX x)
            ++ rotateX x
            ++ rotateY x
            ++ rotateZ x
      )

rotateX :: (Num a) => Vector a -> [Vector a]
rotateX = take 4 . iterate rotX90

rotateY :: (Num a) => Vector a -> [Vector a]
rotateY = take 4 . iterate rotY90

rotateZ :: (Num a) => Vector a -> [Vector a]
rotateZ = take 4 . iterate rotZ90
