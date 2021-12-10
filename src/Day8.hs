module Day8 where

import Data.List (nub, sort)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)

newtype Output = Output [String] deriving (Show, Eq)

newtype Signal = Signal [String] deriving (Show, Eq)

data Entry = Entry Signal Output deriving (Show, Eq)

solve :: [[Char]] -> Int
solve = sum . map ((length . matching) . extractEntry)

extractEntry :: [Char] -> Entry
extractEntry s = Entry (Signal signal) (Output output)
  where
    signal = words . head . splitByPipe $ s
    output = words . last . splitByPipe $ s
    splitByPipe s = splitOn "|" s

matching :: Entry -> [[Char]]
matching (Entry (Signal signal) (Output output)) = filter' (uniques signal) (uniques output)
  where
    filter' [] l = l
    filter' (x : xs) l =
      filter (\w -> sort x == sort w) l ++ filter' xs (filter (\w -> sort x /= sort w) l)

uniques :: [String] -> [String]
uniques = sort . filter (\x -> length x `elem` uniques')
  where
    uniques' = [2, 3, 4, 7]
