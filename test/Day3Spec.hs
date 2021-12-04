module Day3Spec where

import Test.Hspec ( it, shouldBe, Spec )
import Day3 (reportPartTwo, report)

spec :: Spec
spec = do
  it "should count number of increases for window size one" $ do
    report (lines "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010") `shouldBe` 198
    reportPartTwo (lines "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010") `shouldBe` 230
