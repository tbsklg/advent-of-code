module Day1Spec where

import Test.Hspec ( it, shouldBe, Spec )
import Day1 (countIncreases)

spec :: Spec
spec = do
  it "should count number of increases for window size one" $ do
    countIncreases 1 "1\n2\n"                 `shouldBe` 1
    countIncreases 1 "1\n2\n1"                `shouldBe` 1
    countIncreases 1 "1\n2\n2\n1"             `shouldBe` 1
    countIncreases 1 "1\n2\n2\n1\n2\n3\n4"    `shouldBe` 4

  it "should count number of increases for window size three" $ do
    countIncreases 3 "199"                                                `shouldBe` 0
    countIncreases 3 "199\n200\n208\n210\n200\n207\n240\n269\n260\n263"   `shouldBe` 5
