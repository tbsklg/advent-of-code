module Day19Spec where

import Day19 (Vector (..), rotX90, rotY90, rotZ90, rotations)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should rotate z axis by 90 degrees" $ do
    rotZ90 (Vector 1 0 0) `shouldBe` Vector 0 (-1) 0
    rotZ90 (Vector (-1) (-1) 1) `shouldBe` Vector (-1) 1 1
    rotZ90 (Vector 8 0 7) `shouldBe` Vector 0 (-8) 7

  it "should rotate x axis by 90 degrees" $ do
    rotX90 (Vector 1 0 0) `shouldBe` Vector 1 0 0
    rotX90 (Vector (-1) (-1) 1) `shouldBe` Vector (-1) 1 1
    rotX90 (Vector 8 0 7) `shouldBe` Vector 8 7 0
    rotX90 (Vector 4 1 0) `shouldBe` Vector 4 0 (-1)

  it "should rotate y axis by 90 degrees" $ do
    rotY90 (Vector 1 0 0) `shouldBe` Vector 0 0 (-1)
    rotY90 (Vector (-1) (-1) 1) `shouldBe` Vector 1 (-1) 1
    rotY90 (Vector 8 0 7) `shouldBe` Vector 7 0 (-8)
    rotY90 (Vector 4 1 0) `shouldBe` Vector 0 1 (-4)

  it "should find all rotations for a given vector" $ do
    rotations [Vector 8 0 7] `shouldBe` [Vector (-8) (-7) 0, Vector (-8) 0 (-7), Vector (-8) 0 7, Vector (-8) 7 0, Vector (-7) 0 8, Vector 0 (-8) (-7), Vector 0 (-8) 7, Vector 0 8 (-7), Vector 0 8 7, Vector 7 0 (-8), Vector 8 (-7) 0, Vector 8 0 (-7), Vector 8 0 7, Vector 8 7 0]