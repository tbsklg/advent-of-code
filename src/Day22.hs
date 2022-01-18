module Day22 where

import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Maybe
import qualified Data.Set as S
import Day19 (offset)

type Range = (Int, Int)

data Cube = Cube {x :: Range, y :: Range, z :: Range} deriving (Show, Ord, Eq)

data Step = Step {isOn :: Bool, cube :: Cube} deriving (Show, Ord, Eq)

solve :: [[Char]] -> Int
solve = onCubes . reboot . extract

extract :: [[Char]] -> [Step]
extract = map step

step :: [Char] -> Step
step raw = Step {isOn = isOn, cube = (Cube {x = x, y = y, z = z})}
  where
    x = ranges . head . splitOn "," . last . words $ raw
    y = ranges . head . tail . splitOn "," . last . words $ raw
    z = ranges . last . splitOn "," . last . words $ raw
    isOn = (==) "on" . head . words $ raw

ranges :: [Char] -> Range
ranges raw = (from, to)
  where
    from = read (head . splitOn ".." $ s) :: Int
    to = read (last . splitOn ".." $ s) :: Int
    s = last . splitOn "=" $ raw

atLeast50 :: [Step] -> [Step]
atLeast50 =
  filter
    ( \Step {cube = Cube {x = x, y = y, z = z}} ->
        isInRange50 x
          && isInRange50 y
          && isInRange50 z
    )

isInRange50 :: Range -> Bool
isInRange50 (x, y) = x >= (-50) && y <= 50

onCubes :: M.Map Cube Int -> Int
onCubes = sum . map (\(oldCount, t) -> cardinality oldCount * t) . M.toList

reboot :: [Step] -> M.Map Cube Int
reboot steps = reboot' steps M.empty
  where
    reboot' [] state = state
    reboot' (x : xs) state
      | isOn x = reboot' xs withNewCube
      | otherwise = reboot' xs overlapsOnly
      where
        withNewCube = M.insertWith (+) (cube x) 1 overlapsOnly

        overlapsOnly = overlaps (cube x) state

overlaps :: Cube -> M.Map Cube Int -> M.Map Cube Int
overlaps cube state = foldl update state (M.keys state)
  where
    update state' cube' = case overlap cube cube' of
      Just newCube -> case M.lookup cube' state of
        Just count -> case M.lookup newCube state' of
          Just oldCount -> M.insert newCube (oldCount - count) state'
          _ -> M.insert newCube (- count) state'
        _ -> error "Invalid state!"
      _ -> state'

overlap :: Cube -> Cube -> Maybe Cube
overlap s s'
  | isOverlapping (x s) (x s') && isOverlapping (y s) (y s') && isOverlapping (z s) (z s') =
    Just
      Cube
        { x = newRange (x s) (x s'),
          y = newRange (y s) (y s'),
          z = newRange (z s) (z s')
        }
  | otherwise = Nothing

newRange :: Range -> Range -> Range
newRange (x, y) (x', y') = (maximum [x, x'], minimum [y, y'])

isOverlapping :: Range -> Range -> Bool
isOverlapping (x, y) (x', y') = y' >= x && x' <= y

cardinality :: Cube -> Int
cardinality oldCount = elements (x oldCount) * elements (y oldCount) * elements (z oldCount)

elements :: Range -> Int
elements (x, y) = abs (y - x) + 1
