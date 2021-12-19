module Day16 where

import Control.Exception.Base (patError)
import Data.Char (digitToInt)
import Debug.Trace (traceShow)
import Numeric (readHex, readInt)
import Text.Printf (printf)

solve :: [Char] -> Int
solve = sum . versions . convert

versions :: [Char] -> [Int]
versions raw
  | packetTypeId raw == 4 = (packetVersion' . literalValue $ raw) : versions (drop (length . literalValue $raw) raw)
  | otherwise = (packetVersion' . operatorValue $ raw) : versions (drop (length . operatorValue $ raw) raw)
  where
    header = take headerLength raw
    packetVersion' x = packetVersion x
    packetTypeId' = packetTypeId raw

convert :: [Char] -> [Char]
convert = concatMap hexToBin

packetVersion :: [Char] -> Int
packetVersion = binToDec . take 3

packetTypeId :: [Char] -> Int
packetTypeId = binToDec . take 3 . drop 3

literalValue :: [Char] -> [Char]
literalValue raw = take headerLength raw ++ (concat . getValues $ drop 6 raw)
  where
    getValues [] = []
    getValues bin
      | startsWithOne = drop 1 . take 5 $ bin : getValues (drop 5 bin)
      | startsWithZero = drop 1 . take 5 $ bin : getValues []
      | otherwise = error "Invalid packet"
      where
        startsWithOne = (==) '1' . head $ bin
        startsWithZero = (==) '0' . head $ bin

headerLength :: Int
headerLength = 6

operatorValue :: [Char] -> [Char]
operatorValue raw = take headerLength raw ++ (concat . getValues $ drop 6 raw)
  where
    getValues [] = []
    getValues bin
      | lengthTypeId bin == '0' = [take (packetLength + 16) bin]
      | lengthTypeId bin == '1' = take (numberOfPackages - 1) . iterate literalValue  $ bin
      | otherwise = error ""
      where
        packetLength = binToDec . take 15 . drop 1 $ bin
        numberOfPackages = binToDec . take 11 . drop 1 $ bin
        lengthTypeId = (!! 0)

hexToBin :: Char -> String
hexToBin c =
  case readHex [c] of
    (x, _) : _ -> printf "%04b" (x :: Int)
    _ -> ""

binToDec :: [Char] -> Int
binToDec = fst . head . readInt 2 (`elem` "01") digitToInt
