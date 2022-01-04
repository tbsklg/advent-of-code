module Day20Spec where

import Test.Hspec (Spec, shouldBe, it)
import Day20 (Coordinate(..), extract, determinePixels, ieaIndex)

spec :: Spec
spec = do
  it "should extract iea and image" $ do
    extract ["..#.#..#####.#.#.#.###.##.....###.##.#..###.####..####", "", "#..#.", "#....", "##..#"] 
        `shouldBe` ("..#.#..#####.#.#.#.###.##.....###.##.#..###.####..####", ["#..#.", "#....", "##..#"])
  
  it "should return relevant pixels from image for given coordinates" $ do
    determinePixels (Coordinate 0 0) ["#..#.", "#....", "##..#"] `shouldBe` ["...", ".#.", ".#."]
    determinePixels (Coordinate 0 1) ["#..#.", "#....", "##..#"] `shouldBe` ["...", "#..", "#.."]
    determinePixels (Coordinate 1 2) ["#..#.", "#....", "##..#"] `shouldBe` ["..#", "...", "#.."]
    determinePixels (Coordinate 0 10) ["#..#.", "#....", "##..#"] `shouldBe` ["...", "...", "..."]
    determinePixels (Coordinate 10 0) ["#..#.", "#....", "##..#"] `shouldBe` ["...", "...", "..."]
    determinePixels (Coordinate 1 0) ["#..#.", "#....", "##..#"] `shouldBe` [".#.", ".#.", ".##"]
    determinePixels (Coordinate 2 1) ["#..#.", "#....", "##..#"] `shouldBe` ["#..", "##.", "..."]

  it "should calculate iea index for relevant pixels" $ do
    ieaIndex ["...", ".#.", ".#."] `shouldBe` 18
    ieaIndex ["...", "#..", "#.."] `shouldBe` 36
    ieaIndex ["...", "...", "..."] `shouldBe` 0
