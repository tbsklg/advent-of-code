module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day19 (solve, solvePartTwo)

main :: IO ()
main = do
  handle <- openFile "app/resources/day19.txt" ReadMode
  contents <- hGetContents handle
  print $ solvePartTwo . lines $ contents
