module Day24Spec where

import qualified Data.Map as M
import Day24 (interpret, parse)
import Test.Hspec (Spec, it, shouldBe, shouldContain, shouldMatchList)

spec :: Spec
spec = do
  it "should interpret single line" $ do
    interpret "inp w" (M.empty, [5]) `shouldBe` (M.fromList [('w', 5)], [])
    interpret "add z 39" (M.fromList [('z', 2)], []) `shouldBe` (M.fromList [('z', 41)], [])
    interpret "mul w -1" (M.fromList [('w', 4)], []) `shouldBe` (M.fromList [('w', -4)], [])
    interpret "div w 2" (M.fromList [('w', 3)], []) `shouldBe` (M.fromList [('w', 1)], [])
    interpret "mod w 3" (M.fromList [('w', 10)], []) `shouldBe` (M.fromList [('w', 1)], [])
    interpret "eql w 3" (M.fromList [('w', 3)], []) `shouldBe` (M.fromList [('w', 1)], [])
    interpret "eql w 3" (M.fromList [('w', 4)], []) `shouldBe` (M.fromList [('w', 0)], [])

  it "should parse some lines" $ do
    parse
      [ "inp x",
        "mul x -1"
      ]
      [5]
      `shouldBe` M.fromList [('x', -5)]

    parse
      [ "inp z",
        "inp x",
        "mul z 3",
        "eql z x"
      ]
      [3, 9]
      `shouldBe` M.fromList [('x', 9), ('z', 1)]

    parse
      [ "inp w",
        "add z w",
        "mod z 2",
        "div w 2",
        "add y w",
        "mod y 2",
        "div w 2",
        "add x w",
        "mod x 2",
        "div w 2",
        "mod w 2"
      ]
      [5]
      `shouldBe` M.fromList [('w', 0), ('x', 1), ('y', 0), ('z', 1)]

  it "should parse instructions for model number" $ do
    parse
      [ "inp w",
        "inp w"
      ]
      [1, 2]
      `shouldBe` M.fromList [('w', 2)]