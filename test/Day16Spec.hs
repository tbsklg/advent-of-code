module Day16Spec where

import Test.Hspec (Spec, shouldBe, it)

import Day16 ( binToDec, convert )
import Day16 (parse, solve)

spec :: Spec
spec = do
    it "should convert hex into binary" $ do
        convert "D2FE28" `shouldBe` "110100101111111000101000"

    it "should convert bin to dec" $ do
        binToDec "011111100101" `shouldBe` 2021

    it "should sum up packet versions" $ do
        solve "8A004A801A8002F478" `shouldBe` 16
        solve "620080001611562C8802118E34" `shouldBe` 12
        solve "C0015000016115A2E0802F182340" `shouldBe` 23
        solve "A0016C880162017C3686B18A3D4780" `shouldBe` 31
    