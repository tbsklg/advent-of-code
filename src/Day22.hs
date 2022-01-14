module Day22 where

import qualified Data.Set as S

import Data.List.Split (splitOn)
import Day19 (offset)

type Cube = (Int, Int, Int)

data Step = Step {isOn :: Bool, x :: (Int, Int), y :: (Int, Int), z :: (Int, Int)} deriving (Show, Ord, Eq)

solve :: [[Char]] -> Int
solve s = 5

extract :: [[Char]] -> [Step]
extract = map step

step :: [Char] -> Step
step raw = Step {isOn = isOn, x = x, y = y, z = z}
  where
    x = ranges . head . splitOn "," . last . words $ raw
    y = ranges . head . tail . splitOn "," . last . words $ raw
    z = ranges . last . splitOn "," . last . words $ raw
    isOn = (==) "on" . head . words $ raw

ranges :: [Char] -> (Int, Int)
ranges raw = (from, to)
  where
    from = read (head . splitOn ".." $ s) :: Int
    to = read (last . splitOn ".." $ s) :: Int
    s = last . splitOn "=" $ raw

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
