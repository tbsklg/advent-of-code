module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Day3 (reportPartTwo, report)

main :: IO ()
main = do
  handle <- openFile "app/resources/day3.txt" ReadMode 
  contents <- hGetContents handle
  print $ reportPartTwo $ lines contents
