module Day22Spec where

import Day22 (Step (..), extract, step)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should extract steps" $ do
    extract ["on x=-24..25,y=-36..8,z=-15..31", "on x=-34..67,y=-1..2,z=-23..78"]
      `shouldBe` [Step {isOn = True, x = (-24, 25), y = (-36, 8), z = (-15, 31)}, Step {isOn = True, x = (-34, 67), y = (-1, 2), z = (-23, 78)}]

  it "should extract step" $ do
    step "on x=-24..25,y=-36..8,z=-15..31" `shouldBe` (Step {isOn = True, x = (-24, 25), y = (-36, 8), z = (-15, 31)})
