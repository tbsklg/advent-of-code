module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day13 (solvePartTwo)

main :: IO ()
main = do
  handle <- openFile "app/resources/day13.txt" ReadMode
  contents <- hGetContents handle
  putStrLn $ solvePartTwo . lines $ contents