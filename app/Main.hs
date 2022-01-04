module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)

main :: IO ()
main = do
  handle <- openFile "app/resources/day20.txt" ReadMode
  contents <- hGetContents handle
  print $ lines $ contents
