module Day19Spec where

import Day19 (Offset (..), Scanner (..), Vector (..), extract, merge, offset, rotate, rotateScanner)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should rotate for given rotation matrix" $ do
    rotate [[1, 0, 0], [0, 1, 0], [0, 0, 1]] (Vector 2 3 4) `shouldBe` (Vector 2 3 4)
    rotate [[0, -1, 0], [1, 0, 0], [0, 0, 1]] (Vector 2 3 4) `shouldBe` (Vector (-3) 2 4)
    rotate [[-1, 0, 0], [0, -1, 0], [0, 0, 1]] (Vector 2 3 4) `shouldBe` (Vector (-2) (-3) 4)
    rotate [[0, 0, 1], [-1, 0, 0], [0, -1, 0]] (Vector 2 3 4) `shouldBe` (Vector 4 (-2) (-3))
    rotate [[0, 0, 1], [0, -1, 0], [1, 0, 0]] (Vector 2 3 4) `shouldBe` (Vector 4 (-3) 2)

  it "should merge Scanners" $ do
    merge (Scanner 0 (Vector 0 0 0) [Vector 1 2 3, Vector 4 5 6]) (Scanner 0 (Vector 0 0 0) [Vector 7 8 9, Vector 10 11 12]) `shouldBe` (Scanner 0 (Vector 0 0 0) [Vector 1 2 3, Vector 4 5 6, Vector 7 8 9, Vector 10 11 12])

  it "should rotate a Scanner" $ do
    rotateScanner (rotate ([[-1, 0, 0], [0, -1, 0], [0, 0, 1]])) (Scanner 0 (Vector 0 0 0) [Vector 1 2 3, Vector 4 5 6]) `shouldBe` (Scanner 0 (Vector (-1) (-1) 1) [Vector (-1) (-2) 3, Vector (-4) (-5) 6])

  it "should calculate offset for two vectors" $ do
    offset (Vector 2 3 4) (Vector 5 6 7) `shouldBe` (Offset (-3) (-3) (-3))
