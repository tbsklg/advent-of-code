module Day2.Position where

import Data.Bits (Bits (xor))

data Move = Forward Int | Up Int | Down Int deriving (Show)

calculatePosition :: [[Char]] -> Int
calculatePosition l = uncurry (*) position
  where
    position = foldl (flip changePosition) (0, 0) . map extractMove $ l

calculatePositionWithAim :: [[Char]] -> Int
calculatePositionWithAim l = fst' position * snd' position
  where
    fst' (x, _, _) = x
    snd' (_, y, _) = y
    position = foldl (flip changePositionWithAim) (0, 0, 0) . map extractMove $ l

withAim :: Foldable t => t Move -> (Int, Int, Int)
withAim = foldl (flip changePositionWithAim) (0, 0, 0)

withoutAim :: Foldable t => p -> t Move -> (Int, Int)
withoutAim l = foldl (flip changePosition) (0, 0)

extractMove :: [Char] -> Move
extractMove raw
  | (==) "forward" . init . init $ raw = Forward moveSize
  | (==) "up" . init . init $ raw = Up moveSize
  | (==) "down" . init . init $ raw = Down moveSize
  | otherwise = error "Move is not supported!"
  where
    moveSize = read [last raw] :: Int

changePosition :: Move -> (Int, Int) -> (Int, Int)
changePosition (Forward i) (h, d) = (h + i, d)
changePosition (Up i) (h, d) = (h, d - i)
changePosition (Down i) (h, d) = (h, d + i)

changePositionWithAim :: Move -> (Int, Int, Int) -> (Int, Int, Int)
changePositionWithAim (Forward i) (h, d, a) = (h + i, d + a * i, a)
changePositionWithAim (Up i) (h, d, a) = (h, d, a - i)
changePositionWithAim (Down i) (h, d, a) = (h, d, a + i)
