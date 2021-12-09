module Day6Spec where

import Day6 (countFishes, populate, fishes, frequencies, sumUp)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should extract Lanternfishes" $ do
    fishes ["3,4,3,1,2"] `shouldBe` [3, 4, 3, 1, 2]

  it "should calculate remaining Lanternfishes" $ do
    countFishes 80 ["3,4,3,1,2"] `shouldBe` 5934

  it "should calculate frequencies" $ do
    frequencies [3, 4, 3, 1, 2] `shouldBe` [(1, 1), (2, 1), (3, 2), (4, 1)]
    frequencies [2, 3, 2, 0, 1] `shouldBe` [(0, 1), (1, 1), (2, 2), (3, 1)]
    frequencies [3, 3, 2, 1, 3, 4] `shouldBe` [(1, 1), (2, 1), (3, 3), (4, 1)]

  it "should calculate sum of all frequencies" $ do
    sumUp [(1, 1), (2, 1), (3, 3), (4, 1)] `shouldBe` 6

  it "should populate by one" $ do
    populate [(1, 1), (2, 1), (3, 2), (4, 1)] `shouldBe` [(0, 1), (1, 1), (2, 2), (3, 1)]
    populate [(0, 1), (1, 1), (2, 2), (3, 1)] `shouldBe` [(0, 1), (1, 2), (2, 1), (6, 1), (8, 1)]
    populate [(0, 4), (1, 5), (2, 2), (3, 1)] `shouldBe` [(0, 5), (1, 2), (2, 1), (6, 4), (8, 4)]
    populate [(0, 4), (1, 0), (2, 2), (3, 1)] `shouldBe` [(0, 0), (1, 2), (2, 1), (6, 4), (8, 4)]
