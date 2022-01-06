module Day21Spec where

import Day21 (Player(..), extract, nextSpace)
import Test.Hspec (Spec, shouldBe, it)

spec :: Spec
spec = do
  it "should extract players" $ do
    extract ["Player 1 starting position: 4", "Player 2 starting position: 8"] 
        `shouldBe` [(Player {pos = 1, space = 4, score = 0, rolls=[]}), (Player {pos = 2, space = 8, score = 0, rolls=[]})]
  
  
  it "should calculate space" $ do
    nextSpace 0 10 `shouldBe` 10
    nextSpace 0 11 `shouldBe` 1
    nextSpace 0 20 `shouldBe` 10
    nextSpace 4 258 `shouldBe` 2