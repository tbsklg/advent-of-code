module Day3Spec where

import Day3 (report, reportPartTwo)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should calculate power consumption" $ do
    report (lines "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010") `shouldBe` 198

  it "should calculate life support rating" $ do
    reportPartTwo (lines "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010") `shouldBe` 230
