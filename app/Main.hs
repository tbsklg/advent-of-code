module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Day7 (leastFuel)

main :: IO ()
main = do
  handle <- openFile "app/resources/day7.txt" ReadMode 
  contents <- hGetContents handle
  print $ leastFuel . lines $ contents
