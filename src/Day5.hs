module Day5 where

import Data.List (group, groupBy, nub, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

data Point = Point Int Int deriving (Show, Eq)

data Coordinate = Coordinate Point Point deriving (Show, Eq)

data Direction = Vertical | Horizontal | Diagonal deriving (Show, Eq)

countOverlappingPoints :: [String] -> Int
countOverlappingPoints =
  greaterThanOne
    . sortByOccurencesDEC
    . groupByOccurences
    . flatten
    . filter (\x -> isHorizontal x || isVertical x)
    . getCoordinates

countOverlappingPointsPartTwo :: [String] -> Int
countOverlappingPointsPartTwo =
  greaterThanOne
    . sortByOccurencesDEC
    . groupByOccurences
    . flatten
    . filter (\x -> isHorizontal x || isVertical x || isDiagonal x)
    . getCoordinates

greaterThanOne :: [Int] -> Int
greaterThanOne = length . filter (> 1)

sortByOccurencesDEC :: [[Point]] -> [Int]
sortByOccurencesDEC = reverse . sort . map length

groupByOccurences :: [Point] -> [[Point]]
groupByOccurences =
  group
    . sortBy (\(Point x1 _) (Point x2 _) -> compare x1 x2)
    . sortBy (\(Point _ y1) (Point _ y2) -> compare y1 y2)

flatten :: [Coordinate] -> [Point]
flatten [] = []
flatten (x : xs)
  | horizontal = flattenHorizontal x ++ flatten xs
  | vertical = flattenVertical x ++ flatten xs
  | diagonal = flattenDiagonal x ++ flatten xs
  | otherwise = []
  where
    horizontal = getDirection x == Horizontal
    vertical = getDirection x == Vertical
    diagonal = getDirection x == Diagonal

flattenHorizontal :: Coordinate -> [Point]
flattenHorizontal (Coordinate (Point x1 y1) (Point x2 y2)) = zipWith Point [x1, x2 ..] [minimum [y1, y2] .. maximum [y1, y2]]

flattenVertical :: Coordinate -> [Point]
flattenVertical (Coordinate (Point x1 y1) (Point x2 y2)) = zipWith Point [minimum [x1, x2] .. maximum [x1, x2]] [y1, y2 ..]

flattenDiagonal :: Coordinate -> [Point]
flattenDiagonal (Coordinate (Point x1 y1) (Point x2 y2))
  | x1 < x2 = zipWith Point [x1 .. x2] calculateY
  | otherwise = zipWith Point [x1, (x1 - 1) .. x2] calculateY
  where
    calculateY
      | y1 < y2 = [y1 .. y2]
      | otherwise = [y1, (y1 - 1) .. y2]

getDirection :: Coordinate -> Direction
getDirection (Coordinate (Point x1 y1) (Point x2 y2))
  | x1 == x2 && y1 /= y2 = Horizontal
  | y1 == y2 && x1 /= x2 = Vertical
  | otherwise = Diagonal

filterHorizontalOrVertical :: [Coordinate] -> [Coordinate]
filterHorizontalOrVertical = filter isHorizontalOrVertical

filterHorizontalOrVerticalOrDiagonal :: [Coordinate] -> [Coordinate]
filterHorizontalOrVerticalOrDiagonal = filter (\x -> isHorizontal x || isVertical x || isDiagonal x)

getCoordinates :: [String] -> [Coordinate]
getCoordinates = filter (not . isSinglePoint) . map createCoordinates

isSinglePoint :: Coordinate -> Bool
isSinglePoint ((Coordinate point1 point2)) = point1 == point2

isDiagonal :: Coordinate -> Bool
isDiagonal (Coordinate (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) == abs (y1 - y2)

isVertical :: Coordinate -> Bool
isVertical (Coordinate (Point x1 y1) (Point x2 y2)) = y1 == y2

isHorizontal :: Coordinate -> Bool
isHorizontal (Coordinate (Point x1 y1) (Point x2 y2)) = x1 == x2

isHorizontalOrVertical :: Coordinate -> Bool
isHorizontalOrVertical (Coordinate (Point x1 y1) (Point x2 y2)) = x1 == x2 || y1 == y2

createCoordinates :: String -> Coordinate
createCoordinates raw = Coordinate firstPoint secondPoint
  where
    firstPoint = createPoint . head . words $ raw
    secondPoint = createPoint . last . words $ raw

createPoint :: String -> Point
createPoint c = Point x y
  where
    x = read (head splitByComma) :: Int
    y = read (last splitByComma) :: Int
    splitByComma = splitOn "," c
