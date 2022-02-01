module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day25 (solve)
  
main :: IO ()
main = do
  handle <- openFile "app/resources/day25.txt" ReadMode
  contents <- hGetContents handle
  print $ solve . lines $ contents
