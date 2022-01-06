module Day21Spec where

import Day21 (Player(..), extract)
import Test.Hspec (Spec, shouldBe, it)

spec :: Spec
spec = do
  it "should extract players" $ do
    extract ["Player 1 starting position: 4", "Player 2 starting position: 8"] 
        `shouldBe` [(Player {pos = 1, start = 4, score = 0}), (Player {pos = 2, start = 8, score = 0})]
