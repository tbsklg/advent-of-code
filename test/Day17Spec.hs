module Day17Spec where

import Test.Hspec (Spec, it, shouldBe)

import Day17 (Target (..), simulate, extract)

spec :: Spec
spec = do
    it "should extract Target from raw data" $ do
        extract "target area: x=20..30, y=-10..-5" `shouldBe` Target {minXPos = 20, maxXPos = 30, minYPos = -10, maxYPos = -5}


    it "should find maximum y position to for target" $ do
        simulate (Target {minXPos = 20, maxXPos = 30, minYPos = -10, maxYPos = -5}) `shouldBe` 45