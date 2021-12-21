module Day16Spec where

import Day16 (Packet (..), PacketData (..), binToDec, convert, parseLiteral, parseOperator, solve, solvePartTwo, versions)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should convert hex into binary" $ do
    convert "D2FE28" `shouldBe` "110100101111111000101000"

  it "should convert bin to dec" $ do
    binToDec "011111100101" `shouldBe` 2021

  it "should parse literal" $ do
    parseLiteral "101111111000101000" `shouldBe` (Literal 2021, "000")

  it "should parse operator for type id 0" $ do
    parseOperator "00000000000110111101000101001010010001001000000000"
      `shouldBe` ( Packets
                     [ Packet {version = 6, typeId = 4, packetData = Literal 10},
                       Packet {version = 2, typeId = 4, packetData = Literal 20}
                     ],
                   "0000000"
                 )

  it "should parse operator for type id 1" $ do
    parseOperator "10000000001101010000001100100000100011000001100000"
      `shouldBe` ( Packets
                     [ Packet {version = 2, typeId = 4, packetData = Literal 1},
                       Packet {version = 4, typeId = 4, packetData = Literal 2},
                       Packet {version = 1, typeId = 4, packetData = Literal 3}
                     ],
                   ""
                 )

  it "should add up version numbers" $ do
    solve "8A004A801A8002F478" `shouldBe` 16
    solve "620080001611562C8802118E34" `shouldBe` 12
    solve "C0015000016115A2E0802F182340" `shouldBe` 23
    solve "A0016C880162017C3686B18A3D4780" `shouldBe` 31

  it "should solve part two" $ do
    solvePartTwo "C200B40A82" `shouldBe` 3
    solvePartTwo "04005AC33890" `shouldBe` 54
    solvePartTwo "880086C3E88112" `shouldBe` 7
    solvePartTwo "CE00C43D881120" `shouldBe` 9
    solvePartTwo "F600BC2D8F" `shouldBe` 0
    solvePartTwo "D8005AC2A8F0" `shouldBe` 1
    solvePartTwo "9C005AC2F8F0" `shouldBe` 0
    solvePartTwo "9C0141080250320F1802104A08" `shouldBe` 1
