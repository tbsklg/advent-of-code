module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day9 (basin)

main :: IO ()
main = do
  handle <- openFile "app/resources/day9.txt" ReadMode
  contents <- hGetContents handle
  print $ basin . lines $ contents
