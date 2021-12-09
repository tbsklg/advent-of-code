module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Day6 (countFishes)

main :: IO ()
main = do
  handle <- openFile "app/resources/day6.txt" ReadMode 
  contents <- hGetContents handle
  print $ countFishes 256 . lines $ contents
