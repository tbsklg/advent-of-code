module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Day8 (solve)

main :: IO ()
main = do
  handle <- openFile "app/resources/day8.txt" ReadMode 
  contents <- hGetContents handle
  print $ solve . lines $ contents
