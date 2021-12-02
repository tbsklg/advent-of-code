module AdventOfCode.SonarSweepSpec where

import Test.Hspec ( it, shouldBe, Spec )
import AdventOfCode.SonarSweep (countIncreasingWindows)

spec :: Spec
spec = do
  it "should count for window size one" $ do
    countIncreasingWindows 1 [1, 2]                   `shouldBe` 1
    countIncreasingWindows 1 [1, 2, 1]                `shouldBe` 1
    countIncreasingWindows 1 [1, 2, 2, 1]             `shouldBe` 1
    countIncreasingWindows 1 [1, 2, 2, 1, 2, 3, 4]    `shouldBe` 4

  it "should count for window size three" $ do
    countIncreasingWindows 3 [607]                                      `shouldBe` 0
    countIncreasingWindows 3 [607, 614]                                 `shouldBe` 1
    countIncreasingWindows 3 [607, 618, 618, 617, 647, 716, 769, 792]   `shouldBe` 5
