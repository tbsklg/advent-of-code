module Day5 where

import Data.List (group, groupBy, nub, sort, sortBy)
import Data.Ord (comparing)
import Data.List.Split (splitOn)

data Point = Point Int Int deriving (Show, Eq)

data Coordinate = Coordinate Point Point deriving (Show, Eq)

data Direction = Vertical | Horizontal | No deriving (Show, Eq)

countOverlappingPoints :: [String] -> Int
countOverlappingPoints =
  greaterThanOne
    . sortByOccurencesDEC
    . groupByOccurences
    . flatten
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
flatten ((Coordinate (Point x1 y1) (Point x2 y2)) : xs)
  | horizontal = zipWith Point [x1, x2 ..] [minimum [y1, y2] .. maximum [y1, y2]] ++ flatten xs
  | vertical = zipWith Point [minimum [x1, x2] .. maximum [x1, x2]] [y1, y2 ..] ++ flatten xs
  | otherwise = error "Direction is not allowed!"
  where
    horizontal = getDirection (Coordinate (Point x1 y1) (Point x2 y2)) == Horizontal
    vertical = getDirection (Coordinate (Point x1 y1) (Point x2 y2)) == Vertical

getDirection :: Coordinate -> Direction
getDirection (Coordinate (Point x1 y1) (Point x2 y2))
  | x1 == x2 && y1 /= y2 = Horizontal
  | y1 == y2 && x1 /= x2 = Vertical
  | otherwise = error "Only horizontal or vertical directions are allowed!"

getCoordinates :: [String] -> [Coordinate]
getCoordinates = filter (not . isSinglePoint) . filter isHorizontalOrVertical . map createCoordinates

isSinglePoint :: Coordinate -> Bool
isSinglePoint ((Coordinate point1 point2)) = point1 == point2

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
