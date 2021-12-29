module Day19Spec where

import Day19 (Point (..), rotX, rotY, rotZ)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should rotate z axis by 90 degrees" $ do
    rotZ (Point {x = 1, y = 0, z = 0}) `shouldBe` (Point {x = 1, y = 0, z = 0})
    rotZ (Point {x = -1, y = -1, z = 1}) `shouldBe` (Point {x = 1, y = 0, z = 0})
    rotZ (Point {x = 8, y = 0, z = 7}) `shouldBe` (Point {x = 1, y = 0, z = 0})

  it "should rotate x axis by 90 degrees" $ do
    rotX (Point {x = 1, y = 0, z = 0}) `shouldBe` (Point {x = 1, y = 0, z = 0})
    rotX (Point {x = -1, y = -1, z = 1}) `shouldBe` (Point {x = 1, y = 0, z = 0})
    rotX (Point {x = 8, y = 0, z = 7}) `shouldBe` (Point {x = 1, y = 0, z = 0})
    rotX (Point {x = 4, y = 1, z = 0}) `shouldBe` (Point {x = 1, y = 0, z = 0})

  it "should rotate y axis by 90 degrees" $ do
    rotY (Point {x = 1, y = 0, z = 0}) `shouldBe` (Point {x = 1, y = 0, z = 0})
    rotY (Point {x = -1, y = -1, z = 1}) `shouldBe` (Point {x = 1, y = 0, z = 0})
    rotY (Point {x = 8, y = 0, z = 7}) `shouldBe` (Point {x = 1, y = 0, z = 0})
    rotY (Point {x = 4, y = 1, z = 0}) `shouldBe` (Point {x = 1, y = 0, z = 0})