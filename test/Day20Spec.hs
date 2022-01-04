module Day20Spec where

import Day20 (extract)
import Test.Hspec (Spec, shouldBe, it)

spec :: Spec
spec = do
  it "should extract iea and image" $ do
    extract ["..#.#..#####.#.#.#.###.##.....###.##.#..###.####..####", "", "#..#.", "#....", "##..#"] 
        `shouldBe` ("..#.#..#####.#.#.#.###.##.....###.##.#..###.####..####", ["#..#.", "#....", "##..#"])
