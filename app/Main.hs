module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day9 (basin)
import Day11 (flashes)

main :: IO ()
main = do
  handle <- openFile "app/resources/day11.txt" ReadMode
  contents <- hGetContents handle
  print $ flashes . lines $ contents
