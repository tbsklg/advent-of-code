module Day22 where

import Data.List.Split (splitOn)

data Step = Step {isOn :: Bool, x :: (Int, Int), y :: (Int, Int), z :: (Int, Int)} deriving (Show, Ord, Eq)

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
