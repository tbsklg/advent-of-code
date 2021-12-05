module Main where

import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Day4 (score)

main :: IO ()
main = do
  handle <- openFile "app/resources/day4.txt" ReadMode 
  contents <- hGetContents handle
  print $  score . lines $ contents
