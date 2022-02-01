module Day25Spec where

import qualified Data.Map as M
import Day25 (east, south)
import Test.Hspec (Spec, it, shouldBe, shouldContain, shouldMatchList)

spec :: Spec
spec = do
  it "should perform east steps for single row" $ do
    east "...>>>>>..." `shouldBe` "...>>>>.>.."
    east "...>>>>.>.." `shouldBe` "...>>>.>.>."
    east "...>>>.>.>." `shouldBe` "...>>.>.>.>"
    east "...>>.>.>.>" `shouldBe` ">..>.>.>.>."
    east ">..>.>.>.>." `shouldBe` ".>..>.>.>.>"
    east ".>..>.>.>.>" `shouldBe` ">.>..>.>.>."
    east ".>v....v.." `shouldBe` ".>v....v.."

  it "should perform south steps" $ do
    south
      [ ".>v....v..",
        ".......>.."
      ]
      `shouldBe` [ ".>.....v..",
                   "..v....>.."
                 ]

    south
      [ "...>...",
        ".......",
        "......>",
        "v.....>",
        "......>",
        ".......",
        "..vvv.."
      ]
      `shouldBe` [ "..v>v..",
                   ".......",
                   "......>",
                   "......>",
                   "v.....>",
                   ".......",
                   "...v..."
                 ]