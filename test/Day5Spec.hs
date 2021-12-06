module Day5Spec where

import Day5 (Coordinate (..), Direction (..), Point (..), countOverlappingPoints, flatten, getCoordinates, getDirection, groupByOccurences, isHorizontalOrVertical, sortByOccurencesDEC)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should extract coordinates from raw data" $
    getCoordinates ["0,9 -> 5,9", "8,0 -> 0,8", "3,1 -> 3,1", "7,3 -> 4,1", "9,4 -> 3,4", "2,2 -> 2,1", "2,2 -> 2,2"]
      `shouldBe` [Coordinate (Point 0 9) (Point 5 9), Coordinate (Point 9 4) (Point 3 4), Coordinate (Point 2 2) (Point 2 1)]

  it "should check if coordinate horizontal or vertical" $ do
    isHorizontalOrVertical (Coordinate (Point 7 3) (Point 7 1)) `shouldBe` True
    isHorizontalOrVertical (Coordinate (Point 3 5) (Point 3 9)) `shouldBe` True
    isHorizontalOrVertical (Coordinate (Point 7 3) (Point 5 1)) `shouldBe` False

  it "should calculate direction" $ do
    getDirection (Coordinate (Point 7 3) (Point 7 1)) `shouldBe` Horizontal
    getDirection (Coordinate (Point 3 7) (Point 1 7)) `shouldBe` Vertical

  it "should flatten coordinates" $
    flatten [Coordinate (Point 0 9) (Point 5 9), Coordinate (Point 9 4) (Point 9 2), Coordinate (Point 2 2) (Point 2 1)]
      `shouldBe` [Point 0 9, Point 1 9, Point 2 9, Point 3 9, Point 4 9, Point 5 9, Point 9 2, Point 9 3, Point 9 4, Point 2 1, Point 2 2]

  it "should group by occurences" $
    groupByOccurences [Point 0 9, Point 1 9, Point 2 2, Point 3 9, Point 3 9, Point 0 9, Point 9 2, Point 9 3, Point 9 4, Point 2 2, Point 2 2]
      `shouldBe` [[Point 0 9, Point 0 9], [Point 1 9], [Point 2 2, Point 2 2, Point 2 2], [Point 3 9, Point 3 9], [Point 9 2], [Point 9 3], [Point 9 4]]

  it "should sort by occurences" $
    sortByOccurencesDEC [[Point 0 9, Point 0 9], [Point 1 9], [Point 2 2, Point 2 2, Point 2 2], [Point 3 9, Point 3 9], [Point 9 2], [Point 9 3], [Point 9 4]]
      `shouldBe` [3, 2, 2, 1, 1, 1, 1]

  it "should count overlapping points" $
    countOverlappingPoints ["0,9 -> 5,9", "8,0 -> 0,8", "9,4 -> 3,4", "2,2 -> 2,1", "7,0 -> 7,4", "6,4 -> 2,0", "0,9 -> 2,9", "3,4 -> 1,4", "0,0 -> 8,8", "5,5 -> 8,2"]
      `shouldBe` 5
