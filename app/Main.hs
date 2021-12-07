module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Day5 (countOverlappingPoints)

main :: IO ()
main = do
  handle <- openFile "app/resources/day5.txt" ReadMode 
  contents <- hGetContents handle
  print $  countOverlappingPoints . lines $ contents
