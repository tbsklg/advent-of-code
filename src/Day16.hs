module Day16 where

import Data.Char (digitToInt)
import Data.List (iterate')
import Numeric (readHex, readInt)
import Text.Printf (printf)

data Packet = Packet {version :: Int, typeId :: Int, packetData :: PacketData} | Nil deriving (Show, Eq, Ord)

data PacketData = Packets [Packet] | Literal Int deriving (Show, Eq, Ord)

solve :: [Char] -> Int
solve =
  versions
    . parsePackets
    . convert

solvePartTwo :: [Char] -> Int
solvePartTwo =
  operations
    . head
    . parsePackets
    . convert

parsePackets :: [Char] -> [Packet]
parsePackets [] = []
parsePackets package = p : parsePackets remaining
  where
    (p, remaining) = createPacket package

createPacket :: [Char] -> (Packet, [Char])
createPacket raw =
  ( Packet
      { version = version,
        typeId = typeId,
        packetData = pd
      },
    next
  )
  where
    (pd, remaining) = parsePackageData typeId (drop headerLength raw)

    next = if remaining == "" || (read remaining :: Int) == 0 then [] else remaining

    header = take headerLength raw
    version = packetVersion header
    typeId = packetTypeId header

parsePackageData :: Int -> [Char] -> (PacketData, [Char])
parsePackageData 4 = parseLiteral
parsePackageData _ = parseOperator

parseLiteral :: [Char] -> (PacketData, [Char])
parseLiteral = parsePackets' []
  where
    parsePackets' values (s : v0 : v1: v2 : v3 : xs)
      | s == '1' = parsePackets' (values ++ [v0, v1, v2, v3]) xs
      | otherwise = (Literal (binToDec (values ++ [v0, v1, v2, v3])), xs)
    parsePackets' _ _ = error "Invalid literal!"

parseOperator :: [Char] -> (PacketData, [Char])
parseOperator (lti : xs)
  | lti == '0' =
    ( Packets
        (parsePackets (take packetLength . drop 15 $ xs)),
      drop (15 + packetLength) xs
    )
  | lti == '1' = (Packets packets, remaining)
  | otherwise = error "Invalid operator packet!"
  where
    (packets, remaining) = parsePacketsTimes numberOfPackages (drop 11 xs)

    packetLength = binToDec . take 15 $ xs
    numberOfPackages = binToDec . take 11 $ xs
parseOperator _ = error "Invalid operator packet!"

parsePacketsTimes :: (Eq a, Num a) => a -> [Char] -> ([Packet], [Char])
parsePacketsTimes 0 xs = ([], xs)
parsePacketsTimes n xs = (packet : packages, remaining')
  where
    (packages, remaining') = parsePacketsTimes (n -1) remaining
    (packet, remaining) = createPacket xs

versions :: [Packet] -> Int
versions (Packet {version = v, packetData = Packets pd} : xs) = versions xs + v + versions pd
versions (Packet {version = v, packetData = Literal _} : xs) = v + versions xs
versions _ = 0

operations :: Packet -> Int
operations Packet {packetData = Literal a} = a
operations Packet {typeId = tId, packetData = Packets pd}
  | tId == 0 = sum values
  | tId == 1 = product values
  | tId == 2 = minimum values
  | tId == 3 = maximum values
  | tId == 5 = if head values > last values then 1 else 0
  | tId == 6 = if head values < last values then 1 else 0
  | tId == 7 = if head values == last values then 1 else 0
  | otherwise = error "Unsupported typeId for literal operation!"
  where
    values = fmap operations pd
operations _ = error "Invalid operation!"

convert :: [Char] -> [Char]
convert = concatMap hexToBin

packetVersion :: [Char] -> Int
packetVersion = binToDec . take 3

packetTypeId :: [Char] -> Int
packetTypeId = binToDec . take 3 . drop 3

headerLength :: Int
headerLength = 6

-- see https://stackoverflow.com/questions/48234356/haskell-library-function-to-convert-hexadecimal-to-binary-notation/48234462#48234462
hexToBin :: Char -> String
hexToBin c =
  case readHex [c] of
    (x, _) : _ -> printf "%04b" (x :: Int)
    _ -> ""

binToDec :: [Char] -> Int
binToDec = fst . head . readInt 2 (`elem` "01") digitToInt
