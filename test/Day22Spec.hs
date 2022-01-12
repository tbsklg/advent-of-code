module Day22Spec where

import Day22 (Step (..), extract, ranges, step)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should extract steps" $ do
    extract ["on x=-24..25,y=-36..8,z=-15..31", "on x=-34..67,y=-1..2,z=-23..78"]
      `shouldBe` [Step {isOn = True, x = (-24, 25), y = (-36, 8), z = (-15, 31)}, Step {isOn = True, x = (-34, 67), y = (-1, 2), z = (-23, 78)}]

  it "should extract step" $ do
    step "on x=-24..25,y=-36..8,z=-15..31" `shouldBe` (Step {isOn = True, x = (-24, 25), y = (-36, 8), z = (-15, 31)})

  it "should extract range" $ do
    ranges "x=10..12" `shouldBe` (10, 12)
    ranges "y=1000..1200" `shouldBe` (1000, 1200)
    ranges "x=-100..-4" `shouldBe` (-100, -4)
    ranges "x=-100..100" `shouldBe` (-100, 100)
