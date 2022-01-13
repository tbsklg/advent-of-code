module Day22Spec where

import Day22 (Step (..), atLeast50, cubes, extract, ranges, step, turnOff, turnOn, numberOfCubes, adjustRange, update, nextStep)
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

  it "should create cubes" $ do
    cubes (Step {isOn = True, x = (1, 2), y = (1, 2), z = (1, 2)}) `shouldBe` [(1, 1, 1), (1, 1, 2), (1, 2, 1), (1, 2, 2), (2, 1, 1), (2, 1, 2), (2, 2, 1), (2, 2, 2)]
    cubes (Step {isOn = True, x = (1, 1), y = (1, 1), z = (1, 1)}) `shouldBe` [(1, 1, 1)]

  it "should turn on additional cubes" $ do
    turnOn [(1, 1, 1), (1, 1, 2), (1, 1, 3)] [(1, 1, 1), (1, 2, 1), (1, 2, 3)] `shouldBe` [(1, 1, 1), (1, 1, 2), (1, 1, 3), (1, 2, 1), (1, 2, 3)]

  it "should filter steps within range of -50 and 50" $ do
    atLeast50 [Step {isOn = True, x = (-24, 20), y = (-50, 50), z = (10, 11)}, Step {isOn = True, x = (-60, 20), y = (-50, 50), z = (10, 11)}] `shouldBe` [Step {isOn = True, x = (-24, 20), y = (-50, 50), z = (10, 11)}]

  it "should turn off cubes" $ do
    turnOff [(1, 1, 1), (1, 1, 2), (1, 1, 3)] [(1, 1, 1), (2, 3, 0)] `shouldBe` [(1, 1, 2), (1, 1, 3)]

  -- it "should count cubes that are on" $ do
  --   countCubes [Step {isOn = True, x = (1, 2), y = (1, 2), z = (1, 2)}, Step {isOn = True, x = (1, 2), y = (1, 2), z = (1, 2)}] `shouldBe` 8

  it "should caculate number of cubes" $ do
    numberOfCubes (Step {isOn = True, x = (1, 2), y = (1, 2), z = (1, 2)}) `shouldBe` 8
    numberOfCubes (Step {isOn = True, x = (-1, 2), y = (-1, 2), z = (-1, 2)}) `shouldBe` 64
    numberOfCubes (Step {isOn = True, x = (0, 2), y = (0, 2), z = (0, 2)}) `shouldBe` 27
    numberOfCubes (Step {isOn = True, x = (1, 2), y = (1, 3), z = (1, 3)}) `shouldBe` 18
  
  it "should calculate new range" $ do
    adjustRange (4,6) (3,5) `shouldBe` (4, 5)
    adjustRange (1,3) (4,5) `shouldBe` (1, 3)
    adjustRange (1,3) (-4, 0) `shouldBe` (1, 3)
  
  it "should calculate next step" $ do
    nextStep (Step {isOn = True, x = (10, 12), y = (10, 12), z = (10, 12)}) (Step {isOn = True, x = (11, 13), y = (11, 13), z = (11, 13)}) `shouldBe` (Step {isOn = True, x = (10, 13), y = (10, 13), z = (10, 13)})
