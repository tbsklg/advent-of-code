module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day20 (solve, solvePartTwo)

main :: IO ()
main = do
  handle <- openFile "app/resources/day20.txt" ReadMode
  contents <- hGetContents handle
  print $ solvePartTwo . lines $ contents
