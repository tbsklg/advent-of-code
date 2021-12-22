module Day17 where

import Data.List.Split (splitOn)

data Target = Target {minXPos :: Int, maxXPos :: Int, minYPos :: Int, maxYPos :: Int} deriving (Show, Eq, Ord)

data Probe = Probe {xVel :: Int, yVel :: Int, xPos :: Int, yPos :: Int} deriving (Show, Eq, Ord)

step :: Probe -> Probe
step Probe {xVel = xVel, yVel = yVel, xPos = xPos, yPos = yPos} =
  Probe {xVel = xVel', yVel = yVel', xPos = xPos', yPos = yPos'}
  where
    xPos' = xPos + xVel
    yPos' = yPos + yVel
    xVel'
      | xVel == 0 = 0
      | xVel > 0 = xVel - 1
      | otherwise = xVel + 1
    yVel' = yVel - 1

simulate :: Target -> Int
simulate Target {minXPos = minX, maxXPos = maxX, minYPos = minY, maxYPos = maxY} = maximum . simulate' $ initial
  where
    simulate' [] = []
    simulate' (x : xs)
      | hitTarget = (maximum . map (\Probe {yPos = y} -> y) $ steps) : simulate' xs
      | otherwise = simulate' xs
      where
        hitTarget =
          (<=) 1
            . length
            . filter (\Probe {xPos = x, yPos = y} -> x >= minX && x <= maxX && y >= minY && y <= maxY)
            $ steps

        steps =
          takeWhile (\Probe {xPos = x, yPos = y} -> x <= maxX && y >= minY)
            . iterate step
            $ x

    initial =
      [ (Probe {xPos = 0, yPos = 0, xVel = xVel, yVel = yVel})
        | xVel <- [0 .. maxX],
          yVel <- [0 .. maxX]
      ]

extract :: [Char] -> Target
extract raw = Target {minXPos = head xBounds, maxXPos = last xBounds, minYPos = head yBounds, maxYPos = last yBounds}
  where
    xBounds = head rawValues
    yBounds = last rawValues

    rawValues =
      map (map (\y -> read y :: Int) . splitOn "..")
        . map (drop 2)
        . splitOn ", "
        . last
        . splitOn "target area: "
        $ raw

solve :: [Char] -> Int
solve = simulate . extract
