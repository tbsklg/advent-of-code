module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day17 (solve, solvePartTwo)

main :: IO ()
main = do
  handle <- openFile "app/resources/day17.txt" ReadMode
  contents <- hGetContents handle
  print $ solvePartTwo $ contents
