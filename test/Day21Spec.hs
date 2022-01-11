module Day21Spec where

import qualified Control.Applicative as Map
import Day21 (Player (..), extract, nextSpace, winsUntil)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should extract players" $ do
    extract ["Player 1 starting position: 4", "Player 2 starting position: 8"]
      `shouldBe` [(Player {pos = 1, space = 4, score = 0}), (Player {pos = 2, space = 8, score = 0})]

  it "should calculate space" $ do
    nextSpace 0 10 `shouldBe` 10
    nextSpace 0 11 `shouldBe` 1
    nextSpace 0 20 `shouldBe` 10
    nextSpace 4 258 `shouldBe` 2

  it "should caluclate wins in universes" $ do
    winsUntil [(Player {pos = 1, space = 1, score = 0}), (Player {pos = 2, space = 1, score = 0})] 1 `shouldBe` (27, 0)
    winsUntil [(Player {pos = 1, space = 1, score = 0}), (Player {pos = 2, space = 2, score = 0})] 1 `shouldBe` (27, 0)
    winsUntil [(Player {pos = 1, space = 1, score = 0}), (Player {pos = 2, space = 2, score = 1})] 2 `shouldBe` (27, 0)
    winsUntil [(Player {pos = 1, space = 1, score = 0}), (Player {pos = 2, space = 1, score = 1})] 5 `shouldBe` (53, 26)