module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Day2 (calculatePosition, calculatePositionWithAim)

main :: IO ()
main = do
  handle <- openFile "app/resources/day2.txt" ReadMode 
  contents <- hGetContents handle
  print $ calculatePositionWithAim contents
