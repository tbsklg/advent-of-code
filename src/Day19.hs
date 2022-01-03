module Day19 where

import Data.Char (isDigit)
import Data.List (intersect, isInfixOf, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Set (fromList, toList)

data Offset a = Offset a a a deriving (Show, Eq, Ord)

data Vector a = Vector a a a deriving (Show, Eq, Ord)

data Scanner a = Scanner Int (Vector a) [Vector a] deriving (Show, Eq, Ord)

solve :: [[Char]] -> Int
solve = countBeacons . findBeacons . extract

solvePartTwo :: [[Char]] -> Int
solvePartTwo raw = maximum distances
  where
    distances = solve' scanners
    solve' [] = []
    solve' (x : xs) = map (`manhattenDistance` x) xs ++ solve' xs

    scanners = getScanners . extract $ raw

manhattenDistance :: (Num a) => Scanner a -> Scanner a -> a
manhattenDistance (Scanner _ (Vector x y z) _) (Scanner _ (Vector x' y' z') _) =
  abs (x - x') + abs (y - y') + abs (z - z')

extract :: [[Char]] -> [Scanner Int]
extract [] = []
extract (x : xs)
  | "---" `isPrefixOf` x = Scanner id position beacons : extract next
  | otherwise = extract xs
  where
    id = read (takeWhile isDigit . dropWhile (not . isDigit) $ x) :: Int
    position = Vector 0 0 0
    beacons = map createVector . takeWhile (/= "") $ xs

    next = dropWhile (/= "") xs

createVector :: [Char] -> Vector Int
createVector raw = Vector x y z
  where
    x = head values
    y = head . tail $ values
    z = last values
    values = map (\x -> read x :: Int) splitByComma
    splitByComma = splitOn "," raw

rotations :: (Num a) => [Vector a -> Vector a]
rotations =
  [ rotate [[1, 0, 0], [0, 1, 0], [0, 0, 1]],
    rotate [[1, 0, 0], [0, 0, -1], [0, 1, 0]],
    rotate [[1, 0, 0], [0, -1, 0], [0, 0, -1]],
    rotate [[1, 0, 0], [0, 0, 1], [0, -1, 0]],
    rotate [[0, -1, 0], [1, 0, 0], [0, 0, 1]],
    rotate [[0, 0, 1], [1, 0, 0], [0, 1, 0]],
    rotate [[0, 1, 0], [1, 0, 0], [0, 0, -1]],
    rotate [[0, 0, -1], [1, 0, 0], [0, -1, 0]],
    rotate [[-1, 0, 0], [0, -1, 0], [0, 0, 1]],
    rotate [[-1, 0, 0], [0, 0, -1], [0, -1, 0]],
    rotate [[-1, 0, 0], [0, 1, 0], [0, 0, -1]],
    rotate [[-1, 0, 0], [0, 0, 1], [0, 1, 0]],
    rotate [[0, 1, 0], [-1, 0, 0], [0, 0, 1]],
    rotate [[0, 0, 1], [-1, 0, 0], [0, -1, 0]],
    rotate [[0, -1, 0], [-1, 0, 0], [0, 0, -1]],
    rotate [[0, 0, -1], [-1, 0, 0], [0, 1, 0]],
    rotate [[0, 0, -1], [0, 1, 0], [1, 0, 0]],
    rotate [[0, 1, 0], [0, 0, 1], [1, 0, 0]],
    rotate [[0, 0, 1], [0, -1, 0], [1, 0, 0]],
    rotate [[0, -1, 0], [0, 0, -1], [1, 0, 0]],
    rotate [[0, 0, -1], [0, -1, 0], [-1, 0, 0]],
    rotate [[0, -1, 0], [0, 0, 1], [-1, 0, 0]],
    rotate [[0, 0, 1], [0, 1, 0], [-1, 0, 0]],
    rotate [[0, 1, 0], [0, 0, -1], [-1, 0, 0]]
  ]

rotate :: (Num a) => [[a]] -> Vector a -> Vector a
rotate rot (Vector x y z) = Vector x' y' z'
  where
    x' = rot !! 0 !! 0 * x + rot !! 0 !! 1 * y + rot !! 0 !! 2 * z
    y' = rot !! 1 !! 0 * x + rot !! 1 !! 1 * y + rot !! 1 !! 2 * z
    z' = rot !! 2 !! 0 * x + rot !! 2 !! 1 * y + rot !! 2 !! 2 * z

countBeacons :: (Num a) => Scanner a -> Int
countBeacons (Scanner _ _ v) = length v

getScanners :: (Num a, Ord a, Show a) => [Scanner a] -> [Scanner a]
getScanners scanners = getScanners' scanner0 others rotations
  where
    getScanners' s0 [] _ = []
    getScanners' s0 (x : xs) [] = getScanners (s0 : xs ++ [x])
    getScanners' s0 (x : xs) (r : rs) = case overlappings s0 rotated of
      Just a -> a : getScanners (merge s0 a : xs)
      Nothing -> getScanners' s0 (x : xs) rs
      where
        rotated = rotateScanner r x

    scanner0 = head . filter (\(Scanner i _ _) -> i == 0) $ scanners
    others = filter (\(Scanner i _ _) -> i /= 0) scanners

findBeacons :: (Num a, Ord a, Show a) => [Scanner a] -> Scanner a
findBeacons scanners = findBeacons' scanner0 others rotations
  where
    findBeacons' s0 [] _ = s0
    findBeacons' s0 (x : xs) [] = findBeacons (s0 : xs ++ [x])
    findBeacons' s0 (x : xs) (r : rs) = case overlappings s0 rotated of
      Just a -> findBeacons (merge s0 a : xs)
      Nothing -> findBeacons' s0 (x : xs) rs
      where
        rotated = rotateScanner r x

    scanner0 = head . filter (\(Scanner i _ _) -> i == 0) $ scanners
    others = filter (\(Scanner i _ _) -> i /= 0) scanners

merge :: Ord a => Scanner a -> Scanner a -> Scanner a
merge (Scanner i c v) (Scanner _ _ v') = Scanner i c merged
  where
    merged = toList . fromList $ (v ++ v')

rotateScanner :: (Num a) => (Vector a -> Vector a) -> Scanner a -> Scanner a
rotateScanner f (Scanner i c v) = Scanner i newC rotated
  where
    rotated = map f v
    newC = f (Vector 1 1 1)

offset :: Num a => Vector a -> Vector a -> Offset a
offset (Vector x y z) (Vector x' y' z') = Offset deltaX deltaY deltaZ
  where
    deltaX = x - x'
    deltaY = y - y'
    deltaZ = z - z'

withOffset :: (Num a, Ord a) => [Vector a] -> Offset a -> [Vector a]
withOffset v (Offset x y z) = map (\(Vector x' y' z') -> Vector (x' + x) (y' + y) (z' + z)) v

overlappings :: (Num a, Ord a) => Scanner a -> Scanner a -> Maybe (Scanner a)
overlappings (Scanner _ _ v) (Scanner i c v') = findBeacons' [(x, y) | x <- v, y <- v']
  where
    findBeacons' [] = Nothing
    findBeacons' ((v1, v2) : xs)
      | length matchings >= 12 = Just (Scanner i (fromOffset offset') withOffset')
      | otherwise = findBeacons' xs
      where
        offset' = offset v1 v2
        withOffset' = toList . fromList . withOffset v' $ offset'

        matchings = v `intersect` withOffset'

fromOffset :: Offset a -> Vector a
fromOffset (Offset x y z) = Vector x y z
