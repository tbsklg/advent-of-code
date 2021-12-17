module Day15Spec where

import Day15 (ColumnIndex (..), Coordinates (..), Grid (..), Point (..), RowIndex (..), neighbours, lowestRisk)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should find right and down neighbours for given coordinates" $ do
    neighbours Coordinates {columnIndex=0, rowIndex = 0} grid `shouldBe` [Point {coordinates = Coordinates {columnIndex = 0, rowIndex = 1}, value = 1},Point {coordinates = Coordinates {columnIndex = 1, rowIndex = 0}, value = 1}]
    neighbours Coordinates {columnIndex=1, rowIndex = 1} grid `shouldBe` [Point {coordinates = Coordinates {columnIndex = 1, rowIndex = 0}, value = 1},Point {coordinates = Coordinates {columnIndex = 1, rowIndex = 2}, value = 1},Point {coordinates = Coordinates {columnIndex = 2, rowIndex = 1}, value = 8},Point {coordinates = Coordinates {columnIndex = 0, rowIndex = 1}, value = 1}]

  it "should move to next neighbour" $ do
    lowestRisk grid `shouldBe` 40

grid = Grid ["1163751742", "1381373672", "2136511328", "3694931569", "7463417111", "1319128137", "1359912421", "3125421639", "1293138521", "2311944581"]
