module Day25Spec where

import qualified Data.Map as M
import Day25 (step)
import Test.Hspec (Spec, it, shouldBe, shouldContain, shouldMatchList)

spec :: Spec
spec = do
  it "should perform next step for single row" $ do
    step "...>>>>>..." `shouldBe` "...>>>>.>.."
    step "...>>>>.>.." `shouldBe` "...>>>.>.>."
    step "...>>>.>.>." `shouldBe` "...>>.>.>.>"
    step "...>>.>.>.>" `shouldBe` ">..>.>.>.>."
    step ">..>.>.>.>." `shouldBe` ".>..>.>.>.>"
    step ".>..>.>.>.>" `shouldBe` ">.>..>.>.>."
