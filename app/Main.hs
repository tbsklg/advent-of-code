module Main where

import Day8 (solvePartTwo)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

main :: IO ()
main = do
  handle <- openFile "app/resources/day8.txt" ReadMode
  contents <- hGetContents handle
  print $ solvePartTwo . lines $ contents
