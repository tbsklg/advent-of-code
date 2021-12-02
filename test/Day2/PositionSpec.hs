module Day2.PositionSpec where

import Test.Hspec ( it, shouldBe, Spec )
import Day2.Position (countIncreasingWindows)

spec :: Spec
spec = do
  it "should calculate position" $ do
      calculatePosition [""]
