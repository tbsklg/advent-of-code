module Day7Spec where

import Day7 (leastFuel, moveByN, parseFuel)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should calculate fuel" $ do
    leastFuel ["16,1,2,0,4,2,7,1,2,14"] `shouldBe` 37

  it "should parse fuel" $ do
    parseFuel ["16,1,2,0,4,2,7,1,2,14"] `shouldBe` [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

  it "should calculate position by one step" $ do
    moveByN 1 [16, 1, 2, 0, 4, 2, 7, 2, 14]
      `shouldBe` [15, 0, 1, 1, 3, 1, 6, 1, 13]

  it "should calculate position by n steps" $ do
    moveByN 3 [16, 1, 2, 0, 4, 2, 7, 2, 14]
      `shouldBe` [13, 2, 1, 3, 1, 1, 4, 1, 11]