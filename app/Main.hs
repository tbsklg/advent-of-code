module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day12 (solve)

main :: IO ()
main = do
  handle <- openFile "app/resources/day12.txt" ReadMode
  contents <- hGetContents handle
  print $ solve . lines $ contents
