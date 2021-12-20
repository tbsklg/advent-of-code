module Day16 where

import Control.Exception.Base (patError)
import Data.Char (digitToInt)
import Data.List (iterate')
import Numeric (readHex, readInt)
import Text.Printf (printf)

solve :: [Char] -> Int
solve raw = sum . parse (convert raw) $ (-1)

parse :: [Char] -> Int -> [Int]
parse [] counter = []
parse package counter
  | not isValid = []
  | counter == 0 = parse package (-1)
  | typeId == 4 = version : literalValue package counter
  | otherwise = version : operatorValue package counter
  where
    header = take headerLength package
    version = packetVersion header
    typeId = packetTypeId header

    isValid = (read package :: Int) /= 0 && package /= ""

literalValue :: [Char] -> Int -> [Int]
literalValue payload counter = getPackages (drop 6 payload)
  where
    getPackages [] = []
    getPackages current
      | startsWithOne = getPackages (drop 5 current)
      | startsWithZero = parse (drop 5 current) (counter - 1)
      | otherwise = error "Invalid packet"
      where
        startsWithOne = (==) '1' . head . drop headerLength $ payload
        startsWithZero = (==) '0' (payload !! max 0 headerLength)

operatorValue :: [Char] -> Int -> [Int]
operatorValue package counter
  | lengthTypeId package == '0' = parse (take packetLength . drop 22 $ package) counter ++ parse (drop (22 + packetLength) package) (counter - 1)
  | lengthTypeId package == '1' = parse (drop 18 package) numberOfPackages
  | otherwise = error "Unsupported operator package!"
  where
    lengthTypeId = (!! 6)

    packetLength = binToDec . take 15 . drop 7 $ package
    numberOfPackages = binToDec . take 11 . drop 7 $ package

convert :: [Char] -> [Char]
convert = concatMap hexToBin

packetVersion :: [Char] -> Int
packetVersion = binToDec . take 3

packetTypeId :: [Char] -> Int
packetTypeId = binToDec . take 3 . drop 3

headerLength :: Int
headerLength = 6

hexToBin :: Char -> String
hexToBin c =
  case readHex [c] of
    (x, _) : _ -> printf "%04b" (x :: Int)
    _ -> ""

binToDec :: [Char] -> Int
binToDec = fst . head . readInt 2 (`elem` "01") digitToInt
