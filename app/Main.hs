module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day15 (solvePartTwo)

main :: IO ()
main = do
  handle <- openFile "app/resources/day15.txt" ReadMode
  contents <- hGetContents handle
  print $ solvePartTwo . lines $ contents
