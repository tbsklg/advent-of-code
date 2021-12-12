module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day9 (basin)
import Day10 (score, scorePartTwo)

main :: IO ()
main = do
  handle <- openFile "app/resources/day10.txt" ReadMode
  contents <- hGetContents handle
  print $ scorePartTwo . lines $ contents
