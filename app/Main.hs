module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day21 (solve, solvePartTwo)

main :: IO ()
main = do
  handle <- openFile "app/resources/day21.txt" ReadMode
  contents <- hGetContents handle
  print $ solvePartTwo . lines $ contents
