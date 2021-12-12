module Day11Spec where

import Test.Hspec (Spec, shouldBe, it)
import Day11 (flashes, setElem, Point (..))

spec :: Spec
spec = do
    it "should count flashes" $ do
      flashes ["5483143223", "2745854711", "5264556173", "6141336146", "6357385478", "4167524645", "2176841721", "6882881134", "4846848554", "5283751526"] `shouldBe` 1656
    
    it "should update value in matrix " $ do
        setElem 8 (Point 0 1) [[1,2,3],[4,5,6]] `shouldBe` [[1,2,3], [8,5,6]]
        setElem 8 (Point 2 1) [[1,2,3],[4,5,6]] `shouldBe` [[1,2,3], [4,5,8]]
        setElem 2 (Point 0 0) [[1,2,3],[4,5,6]] `shouldBe` [[2,2,3], [4,5,6]]