module Day19 where

-- see https://de.wikipedia.org/wiki/Eulersche_Winkel

rotZ90 :: [[Int]]
rotZ90 = [[0, -1, 0], [1, 0, 0], [0, 0, 1]]

rotX90 :: [[Int]]
rotX90 = [[1, 0, 0], [0, 0, -1], [0, 1, 0]]

rotY90 :: [[Int]]
rotY90 = [[0, 0, 1], [0, 1, 0], [-1, 0, 0]]

rotZ :: [Int] -> [Int]
rotZ x = multiply x rotZ90

rotX :: [Int] -> [Int]
rotX x = multiply x rotX90

rotY :: [Int] -> [Int]
rotY x = multiply x rotY90

multiply :: [Int] -> [[Int]] -> [Int]
multiply [x, y, z] rot = [x', y', z']
  where
    x' = rot !! 0 !! 0 * x + rot !! 0 !! 1 * y + rot !! 0 !! 2 * z
    y' = rot !! 1 !! 0 * x + rot !! 1 !! 1 * y + rot !! 1 !! 2 * z
    z' = rot !! 2 !! 0 * x + rot !! 2 !! 1 * y + rot !! 2 !! 2 * z
multiply _ _ = error "Only 3d point allowed"

rotateZ :: [[Int]] -> [[Int]]
rotateZ = map rotZ
