module Day22 where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import Day19 (offset)
import Debug.Trace

type Cube = (Int, Int, Int)

data Step = Step {isOn :: Bool, x :: (Int, Int), y :: (Int, Int), z :: (Int, Int)} deriving (Show, Ord, Eq)

solve :: [[Char]] -> Int
solve = countCubes .atLeast50 . extract

extract :: [[Char]] -> [Step]
extract = map step

countCubes :: [Step] -> Int
countCubes = length . foldl update []

update :: [Cube] -> Step -> [Cube]
update c s
  | isOn s = turnOn c . cubes $ s
  | otherwise = turnOff c . cubes $ s

step :: [Char] -> Step
step raw = Step {isOn = isOn, x = x, y = y, z = z}
  where
    x = ranges . head . splitOn "," . last . words $ raw
    y = ranges . head . tail . splitOn "," . last . words $ raw
    z = ranges . last . splitOn "," . last . words $ raw
    isOn = (==) "on" . head . words $ raw

numberOfCubes :: Step -> Int
numberOfCubes Step {x = (fx, tx), y = (fy, ty), z = (fz, tz) } = (tx - fx + 1) * (ty - fy + 1) * (tz - fz + 1)

ranges :: [Char] -> (Int, Int)
ranges raw = (from, to)
  where
    from = read (head . splitOn ".." $ s) :: Int
    to = read (last . splitOn ".." $ s) :: Int
    s = last . splitOn "=" $ raw

cubes :: Step -> [Cube]
cubes Step {x = (fx, tx), y = (fy, ty), z = (fz, tz)} = [(x', y', z') | x' <- [fx .. tx], y' <- [fy .. ty], z' <- [fz .. tz]]

turnOn :: [Cube] -> [Cube] -> [Cube]
turnOn c1 c2 = S.toList . S.fromList $ (c1 ++ c2)

turnOff :: [Cube] -> [Cube] -> [Cube]
turnOff c1 c2 = filter (`notElem` c2) c1

atLeast50 :: [Step] -> [Step]
atLeast50 =
  filter
    ( \Step {x = x, y = y, z = z} ->
        isInRange50 x
          && isInRange50 y
          && isInRange50 z
    )

isInRange50 :: (Int, Int) -> Bool
isInRange50 (x, y) = x >= (-50) && y <= 50
