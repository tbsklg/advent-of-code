module Day16Spec where

import Test.Hspec (Spec, shouldBe, it)
import Day16

spec :: Spec
spec = do
    it "should convert hex into binary" $ do
        convert "D2FE28" `shouldBe` "110100101111111000101000"

    it "should extract packet version" $ do
        packetVersion "110100" `shouldBe` 6

    it "should extract packet type id" $ do
        packetTypeId "110100" `shouldBe` 4

    it "should calculate literal value" $ do
        literalValue "110100" `shouldBe` "110100"

    it "should convert bin to dec" $ do
        binToDec "011111100101" `shouldBe` 2021

    it "should calculate the opertator value" $ do
        operatorValue "00111000000000000110111101000101001010010001001000000000" `shouldBe` "0011100000000000011011110100010100101001000100100"
        operatorValue "11101110000000001101010000001100100000100011000001100000" `shouldBe` "11101110000000001101010000001100100000100011000001100000100000"