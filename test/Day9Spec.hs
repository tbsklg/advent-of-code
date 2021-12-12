module Day9Spec where

import Day9 (Point (..), basin, basins, riskLevel, riskLevels)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should calculate risk level" $ do
    riskLevel ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"] `shouldBe` 15

  it "should calculate the risk levels" $ do
    riskLevels ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"] `shouldBe` [(Point 1 0, 1), (Point 9 0, 0), (Point 2 2, 5), (Point 6 4, 5)]

  it "should calculate the product of the three largest basins" $ do
    basin ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"] `shouldBe` 1134

  it "should calculate the basins" $ do
    basins (Point 1 0, 1) ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"] `shouldBe` [Point 0 0, Point 0 1, Point 1 0]
    basins (Point 9 0, 0) ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"] `shouldBe` [Point 5 0, Point 6 0, Point 6 1, Point 7 0, Point 8 0, Point 8 1, Point 9 0, Point 9 1, Point 9 2]
    basins (Point 2 2, 5) ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"] `shouldBe` [Point 0 3, Point 1 2, Point 1 3, Point 1 4, Point 2 1, Point 2 2, Point 2 3, Point 3 1, Point 3 2, Point 3 3, Point 4 1, Point 4 2, Point 4 3, Point 5 2]
    basins (Point 6 4, 5) ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"] `shouldBe` [Point 5 4, Point 6 3, Point 6 4, Point 7 2, Point 7 3, Point 7 4, Point 8 3, Point 8 4, Point 9 4]
