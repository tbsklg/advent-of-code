module Day19Spec where

import Day19 (rotX, rotY, rotZ)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should rotate z axis by 90 degrees" $ do
    rotZ [1, 0, 0] `shouldBe` [0, 1, 0]
    rotZ [-1, -1, 1] `shouldBe` [1, -1, 1]
    rotZ [8, 0, 7] `shouldBe` [0, 8, 7]

  it "should rotate x axis by 90 degrees" $ do
    rotX [1, 0, 0] `shouldBe` [1, 0, 0]
    rotX [-1, -1, 1] `shouldBe` [-1, -1, -1]
    rotX [1, -1, 1] `shouldBe` [1, -1, -1]
    rotX [8, 0, 7] `shouldBe` [8, -7, 0]

  it "should rotate y axis by 90 degrees" $ do
    rotY [1, 0, 0] `shouldBe` [0, 0, -1]
    rotY [-1, -1, 1] `shouldBe` [1, -1, 1]
    rotY [1, -1, 1] `shouldBe` [1, -1, -1]
    rotY [8, -7, 0] `shouldBe` [0, -7, -8]