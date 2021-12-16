module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day14 (solve)

main :: IO ()
main = do
  handle <- openFile "app/resources/day14.txt" ReadMode
  contents <- hGetContents handle
  print $ solve 10 . lines $ contents