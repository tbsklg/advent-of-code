module Day8 where

import Data.List (elemIndex, nub, permutations, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

newtype Output = Output [String] deriving (Show, Eq)

newtype Signal = Signal [String] deriving (Show, Eq)

data Entry = Entry Signal Output deriving (Show, Eq)

solve :: [String] -> Int
solve = sum . map ((length . matching) . extractEntry)

solvePartTwo :: [String] -> Int
solvePartTwo = sum . map (\x -> read x :: Int) . map decode . map extractEntry

decode :: Entry -> String
decode (Entry (Signal signal) (Output output)) = concatMap ((show . digit) . (`decodeWithKey` key)) output
  where
    key = findKey signal
    digit x = fromMaybe (- 1) (elemIndex x digits)

extractEntry :: String -> Entry
extractEntry s = Entry (Signal signal) (Output output)
  where
    signal = words . head . splitByPipe $ s
    output = words . last . splitByPipe $ s
    splitByPipe s = splitOn "|" s

findKey :: [String] -> String
findKey s = findKey' (permutations "abcdefg")
  where
    findKey' [] = error "Could not find a matching key"
    findKey' (key : xs)
      | foldl (\y w -> y && decodeWithKey w key `elem` digits) True s = key
      | otherwise = findKey' xs

decodeWithKey :: String -> String -> String
decodeWithKey s key = sort . concatMap dict $ s
  where
    dict x = map snd . filter (\(from, _) -> from == x) . zip ['a' .. 'g'] $ key

digits :: [String]
digits = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

matching :: Entry -> [String]
matching (Entry (Signal signal) (Output output)) = filter' (uniques signal) (uniques output)
  where
    filter' [] l = l
    filter' (x : xs) l =
      filter (\w -> sort x == sort w) l ++ filter' xs (filter (\w -> sort x /= sort w) l)

uniques :: [String] -> [String]
uniques = sort . filter (\x -> length x `elem` uniques')
  where
    uniques' = [2, 3, 4, 7]
