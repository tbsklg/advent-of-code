module Day19 where

import Data.Set (fromList, toList)

data Point = Point {x :: Int, y :: Int, z :: Int} deriving (Show, Eq, Ord)

rotZ90 :: Point -> Point
rotZ90 Point {x = x, y = y, z = z} = Point {x = y, y = x * (-1), z = z}

rotX90 :: Point -> Point
rotX90 Point {x = x, y = y, z = z} = Point {x = x, y = z, z = y * (-1)}

rotY90 :: Point -> Point
rotY90 Point {x = x, y = y, z = z} = Point {x = z, y = y, z = x * (-1)}

flipX :: Point -> Point
flipX Point {x = x, y = y, z = z} = Point {x = x * (-1), y = y, z = z}

flipY :: Point -> Point
flipY Point {x = x, y = y, z = z} = Point {x = x, y = y * (-1), z = z}

flipZ :: Point -> Point
flipZ Point {x = x, y = y, z = z} = Point {x = x, y = y, z = z * (-1)}

rotations :: [Point] -> [Point]
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

rotateX :: Point -> [Point]
rotateX = take 4 . iterate rotX90

rotateY :: Point -> [Point]
rotateY = take 4 . iterate rotY90

rotateZ :: Point -> [Point]
rotateZ = take 4 . iterate rotZ90
