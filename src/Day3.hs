module Day3 (report, reportPartTwo) where

import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)
import Numeric (readInt)

report :: [[Char]] -> Int
report lines = gamma * epsilon
  where
    gamma = toDecimal . powerConsumption $ lines
    epsilon = toDecimal . complement . powerConsumption $ lines

reportPartTwo :: [[Char]] -> Int
reportPartTwo lines = oxigen * co2
  where
    oxigen = toDecimal . oxigenGeneratorRating $ lines
    co2 = toDecimal . co2ScrubberRating $ lines

powerConsumption :: [[Char]] -> [Char]
powerConsumption [] = []
powerConsumption (x : xs) = power' 0 (x : xs)
  where
    power' _ [] = []
    power' index (x : xs)
      | index == length x = []
      | otherwise = (fromMaybe '1' . mostCommonBits index $ (x : xs)) : power' (index + 1) (x : xs)

oxigenGeneratorRating :: [[Char]] -> [Char]
oxigenGeneratorRating [] = []
oxigenGeneratorRating r = mostCommonBit : oxigenGeneratorRating nextReport
  where
    mostCommonBit = fromMaybe '1' . mostCommonBits 0 $ r
    nextReport = filter (/= "") . map tail . filter (\x -> maybeHead x == Just mostCommonBit) $ r

co2ScrubberRating :: [[Char]] -> [Char]
co2ScrubberRating [r] = r
co2ScrubberRating r = leastCommonBit : co2ScrubberRating nextReport
  where
    leastCommonBit = fromMaybe '0' . leastCommonBits 0 $ r
    nextReport = map tail . filter (\x -> maybeHead x == Just leastCommonBit) $ r

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

mostCommonBits :: Int -> [[Char]] -> Maybe Char
mostCommonBits index lines
  | ones > zeros = Just '1'
  | zeros > ones = Just '0'
  | otherwise = Nothing
  where
    ones = length . filter (\x -> x !! index == '1') $ lines
    zeros = length . filter (\x -> x !! index == '0') $ lines

leastCommonBits :: Int -> [[Char]] -> Maybe Char
leastCommonBits index lines
  | ones < zeros = Just '1'
  | zeros < ones = Just '0'
  | otherwise = Nothing
  where
    ones = length . filter (\x -> x !! index == '1') $ lines
    zeros = length . filter (\x -> x !! index == '0') $ lines

complement :: [Char] -> [Char]
complement [] = []
complement (x : xs)
  | x == '1' = '0' : complement xs
  | otherwise = '1' : complement xs

toDecimal :: String -> Int
toDecimal = fst . head . readInt 2 (`elem` "01") digitToInt
