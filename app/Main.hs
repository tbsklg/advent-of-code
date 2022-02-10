module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day18 (solve, solvePartTwo)
  
main :: IO ()
main = do
  handle <- openFile "app/resources/day18.txt" ReadMode
  contents <- hGetContents handle
  print $ solvePartTwo . lines $ contents
