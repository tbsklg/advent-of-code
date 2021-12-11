module Day9Spec where

import Day9 (riskLevel, riskLevels)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should calculate risk level" $ do
    riskLevel ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"] `shouldBe` 15

  it "should calculate risk level" $ do
    riskLevels ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"] `shouldBe` [1, 0, 5, 5]
