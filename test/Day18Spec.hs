module Day18Spec where

import Day18 (Crumb (LeftCrumb, RightCrumb), Tree (..), magnitude, add, explode, nearestLeft, maxDepth', pairAtDepth, reduce, nearestRight, slrParse, split, splitAt')
import Test.Hspec (Spec, anyException, errorCall, it, shouldBe, shouldThrow)

spec :: Spec
spec = do
  it "should perform slr parsing" $ do
    slrParse "[1,2]" `shouldBe` Node (Leaf 1) (Leaf 2)
    slrParse "[[1,2],3]" `shouldBe` Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
    slrParse "[9,[8,7]]" `shouldBe` Node (Leaf 9) (Node (Leaf 8) (Leaf 7))
    slrParse "[[1,9],[8,5]]" `shouldBe` Node (Node (Leaf 1) (Leaf 9)) (Node (Leaf 8) (Leaf 5))
    slrParse "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]" `shouldBe` Node (Node (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))) (Node (Node (Leaf 5) (Leaf 6)) (Node (Leaf 7) (Leaf 8)))) (Leaf 9)
    slrParse "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]" `shouldBe` Node (Node (Node (Leaf 9) (Node (Leaf 3) (Leaf 8))) (Node (Node (Leaf 0) (Leaf 9)) (Leaf 6))) (Node (Node (Node (Leaf 3) (Leaf 7)) (Node (Leaf 4) (Leaf 9))) (Leaf 3))
    slrParse "[[[[[9,8],1],2],3],4]" `shouldBe` Node (Node (Node (Node (Node (Leaf 9) (Leaf 8)) (Leaf 1)) (Leaf 2)) (Leaf 3)) (Leaf 4)
    slrParse "[7,[6,[5,[4,[3,2]]]]]" `shouldBe` Node (Leaf 7) (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2)))))
    slrParse "[[6,[5,[4,[3,2]]]],1]" `shouldBe` Node (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2))))) (Leaf 1)
    slrParse "[[6,[5,[4,[3,2]]]],1]" `shouldBe` Node (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2))))) (Leaf 1)
    slrParse "[7,[[8,4],9]]" `shouldBe` Node (Leaf 7) (Node (Node (Leaf 8) (Leaf 4)) (Leaf 9))
    slrParse "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]" `shouldBe` Node (Node (Node (Node (Node (Leaf 4) (Leaf 3)) (Leaf 4)) (Leaf 4)) (Node (Leaf 7) (Node (Node (Leaf 8) (Leaf 4)) (Leaf 9)))) (Node (Leaf 1) (Leaf 1))

  it "should add two snailfish numbers" $ do
    add (Node (Leaf 1) (Leaf 2)) (Node (Node (Leaf 3) (Leaf 4)) (Leaf 5)) `shouldBe` Node (Node (Leaf 1) (Leaf 2)) (Node (Node (Leaf 3) (Leaf 4)) (Leaf 5))

  it "should calculate max depth of tree" $ do
    maxDepth' (Node (Leaf 1) (Leaf 2)) `shouldBe` 1
    maxDepth' (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)) `shouldBe` 2
    maxDepth' (Node (Node (Node (Node (Node (Leaf 9) (Leaf 8)) (Leaf 1)) (Leaf 2)) (Leaf 3)) (Leaf 4)) `shouldBe` 5
    maxDepth' (Node (Node (Node (Leaf 9) (Node (Leaf 3) (Leaf 8))) (Node (Node (Leaf 0) (Leaf 9)) (Leaf 6))) (Node (Node (Node (Leaf 3) (Leaf 7)) (Node (Leaf 4) (Leaf 9))) (Leaf 3))) `shouldBe` 4

  it "should reduce" $ do
    reduce (Node (Node (Node (Node (Node (Leaf 4) (Leaf 3)) (Leaf 4)) (Leaf 4)) (Node (Leaf 7) (Node (Node (Leaf 8) (Leaf 4)) (Leaf 9)))) (Node (Leaf 1) (Leaf 1)), []) `shouldBe` (Node (Node (Node (Node (Leaf 0) (Leaf 7)) (Leaf 4)) (Node (Node (Leaf 7) (Leaf 8)) (Node (Leaf 6) (Leaf 0)))) (Node (Leaf 8) (Leaf 1)),[])

  it "should explode" $ do
    explode (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3), []) `shouldBe` Nothing
    explode (Node (Node (Node (Node (Node (Leaf 9) (Leaf 8)) (Leaf 1)) (Leaf 2)) (Leaf 3)) (Leaf 4), []) `shouldBe` Just (Node (Node (Node (Node (Leaf 0) (Leaf 9)) (Leaf 2)) (Leaf 3)) (Leaf 4), [])
    explode (Node (Leaf 7) (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2))))), []) `shouldBe` Just (Node (Leaf 7) (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 7) (Leaf 0)))), [])
    explode (Node (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2))))) (Leaf 1), []) `shouldBe` Just (Node (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 7) (Leaf 0)))) (Leaf 3), [])
    explode (Node (Node (Leaf 3) (Node (Leaf 2) (Node (Leaf 1) (Node (Leaf 7) (Leaf 3))))) (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2))))), []) `shouldBe` Just (Node (Node (Leaf 3) (Node (Leaf 2) (Node (Leaf 8) (Leaf 0)))) (Node (Leaf 9) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2))))), [])
    explode (Node (Node (Leaf 3) (Node (Leaf 2) (Node (Leaf 8) (Leaf 0)))) (Node (Leaf 9) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2))))), []) `shouldBe` Just (Node (Node (Leaf 3) (Node (Leaf 2) (Node (Leaf 8) (Leaf 0)))) (Node (Leaf 9) (Node (Leaf 5) (Node (Leaf 7) (Leaf 0)))), [])

  it "should find pairs at depth 4" $ do
    pairAtDepth 4 (Node (Node (Node (Node (Node (Leaf 9) (Leaf 8)) (Leaf 1)) (Leaf 2)) (Leaf 3)) (Leaf 4), []) `shouldBe` Just (Node (Leaf 9) (Leaf 8), [LeftCrumb (Leaf 1), LeftCrumb (Leaf 2), LeftCrumb (Leaf 3), LeftCrumb (Leaf 4)])
    pairAtDepth 4 (Node (Leaf 7) (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2))))), []) `shouldBe` Just (Node (Leaf 3) (Leaf 2), [RightCrumb (Leaf 4), RightCrumb (Leaf 5), RightCrumb (Leaf 6), RightCrumb (Leaf 7)])
    pairAtDepth 4 (Node (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2))))) (Leaf 1), []) `shouldBe` Just (Node (Leaf 3) (Leaf 2), [RightCrumb (Leaf 4), RightCrumb (Leaf 5), RightCrumb (Leaf 6), LeftCrumb (Leaf 1)])
    pairAtDepth 4 (Node (Node (Leaf 3) (Node (Leaf 2) (Node (Leaf 1) (Node (Leaf 7) (Leaf 3))))) (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2))))), []) `shouldBe` Just (Node (Leaf 7) (Leaf 3), [RightCrumb (Leaf 1), RightCrumb (Leaf 2), RightCrumb (Leaf 3), LeftCrumb (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2)))))])
    pairAtDepth 4 (Node (Node (Leaf 3) (Node (Leaf 2) (Node (Leaf 8) (Leaf 0)))) (Node (Leaf 9) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2))))), []) `shouldBe` Just (Node (Leaf 3) (Leaf 2), [RightCrumb (Leaf 4), RightCrumb (Leaf 5), RightCrumb (Leaf 9), RightCrumb (Node (Leaf 3) (Node (Leaf 2) (Node (Leaf 8) (Leaf 0))))])

  it "should find next left" $ do
    nearestLeft (Node (Leaf 9) (Leaf 8), [LeftCrumb (Leaf 1), LeftCrumb (Leaf 2), LeftCrumb (Leaf 3), LeftCrumb (Leaf 4)]) `shouldBe` Nothing
    nearestLeft (Node (Leaf 3) (Leaf 2), [RightCrumb (Leaf 4), RightCrumb (Leaf 5), RightCrumb (Leaf 6), RightCrumb (Leaf 7)]) `shouldBe` Just (Leaf 4, [LeftCrumb (Node (Leaf 3) (Leaf 2)), RightCrumb (Leaf 5), RightCrumb (Leaf 6), RightCrumb (Leaf 7)])
    nearestLeft (Node (Leaf 3) (Leaf 2), [RightCrumb (Leaf 4), RightCrumb (Leaf 5), RightCrumb (Leaf 6), LeftCrumb (Leaf 1)]) `shouldBe` Just (Leaf 4, [LeftCrumb (Node (Leaf 3) (Leaf 2)), RightCrumb (Leaf 5), RightCrumb (Leaf 6), LeftCrumb (Leaf 1)])
    nearestLeft (Node (Leaf 7) (Leaf 3), [RightCrumb (Leaf 1), RightCrumb (Leaf 2), RightCrumb (Leaf 3), LeftCrumb (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2)))))]) `shouldBe` Just (Leaf 1, [LeftCrumb (Node (Leaf 7) (Leaf 3)), RightCrumb (Leaf 2), RightCrumb (Leaf 3), LeftCrumb (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2)))))])
    nearestLeft (Node (Leaf 3) (Leaf 2), [RightCrumb (Leaf 4), RightCrumb (Leaf 5), RightCrumb (Leaf 9), RightCrumb (Node (Leaf 3) (Node (Leaf 2) (Node (Leaf 8) (Leaf 0))))]) `shouldBe` Just (Leaf 4, [LeftCrumb (Node (Leaf 3) (Leaf 2)), RightCrumb (Leaf 5), RightCrumb (Leaf 9), RightCrumb (Node (Leaf 3) (Node (Leaf 2) (Node (Leaf 8) (Leaf 0))))])

  it "should find next right" $ do
    nearestRight (Node (Leaf 9) (Leaf 8), [LeftCrumb (Leaf 1), LeftCrumb (Leaf 2), LeftCrumb (Leaf 3), LeftCrumb (Leaf 4)]) `shouldBe` Just (Leaf 1, [RightCrumb (Node (Leaf 9) (Leaf 8)), LeftCrumb (Leaf 2), LeftCrumb (Leaf 3), LeftCrumb (Leaf 4)])
    nearestRight (Node (Leaf 3) (Leaf 2), [RightCrumb (Leaf 4), RightCrumb (Leaf 5), RightCrumb (Leaf 6), RightCrumb (Leaf 7)]) `shouldBe` Nothing
    nearestRight (Node (Leaf 3) (Leaf 2), [RightCrumb (Leaf 4), RightCrumb (Leaf 5), RightCrumb (Leaf 6), LeftCrumb (Leaf 1)]) `shouldBe` Just (Leaf 1, [RightCrumb (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2)))))])
    nearestRight (Node (Leaf 7) (Leaf 3), [RightCrumb (Leaf 1), RightCrumb (Leaf 2), RightCrumb (Leaf 3), LeftCrumb (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2)))))]) `shouldBe` Just (Leaf 6, [LeftCrumb (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2)))), RightCrumb (Node (Leaf 3) (Node (Leaf 2) (Node (Leaf 1) (Node (Leaf 7) (Leaf 3)))))])
    nearestRight (Node (Leaf 3) (Leaf 2), [RightCrumb (Leaf 4), RightCrumb (Leaf 5), RightCrumb (Leaf 6), LeftCrumb (Leaf 1)]) `shouldBe` Just (Leaf 1, [RightCrumb (Node (Leaf 6) (Node (Leaf 5) (Node (Leaf 4) (Node (Leaf 3) (Leaf 2)))))])

  it "should find leaf to split" $ do
    splitAt' (Node (Leaf 10) (Leaf 5), []) `shouldBe` Just (Leaf 10, [LeftCrumb (Leaf 5)])
    splitAt' (Node (Leaf 5) (Leaf 20), []) `shouldBe` Just (Leaf 20, [RightCrumb (Leaf 5)])
    splitAt' (Node (Node (Node (Node (Node (Leaf 9) (Leaf 8)) (Leaf 1)) (Leaf 2)) (Leaf 56)) (Leaf 4), []) `shouldBe` Just (Leaf 56, [RightCrumb (Node (Node (Node (Leaf 9) (Leaf 8)) (Leaf 1)) (Leaf 2)), LeftCrumb (Leaf 4)])
    splitAt' (Node (Node (Node (Node (Node (Leaf 32) (Leaf 8)) (Leaf 4)) (Leaf 2)) (Leaf 6)) (Leaf 4), []) `shouldBe` Just (Leaf 32, [LeftCrumb (Leaf 8), LeftCrumb (Leaf 4), LeftCrumb (Leaf 2), LeftCrumb (Leaf 6), LeftCrumb (Leaf 4)])

  it "should split" $ do
    split (Node (Leaf 10) (Leaf 5), []) `shouldBe` Just (Node (Node (Leaf 5) (Leaf 5)) (Leaf 5), [])
    split (Node (Leaf 3) (Leaf 20), []) `shouldBe` Just (Node (Leaf 3) (Node (Leaf 10) (Leaf 10)),[])
    split (Node (Leaf 20) (Leaf 20), []) `shouldBe` Just (Node (Node (Leaf 10) (Leaf 10)) (Leaf 20),[])
    split (Node (Node (Node (Node (Node (Leaf 32) (Leaf 8)) (Leaf 4)) (Leaf 2)) (Leaf 6)) (Leaf 4), []) `shouldBe` Just (Node (Node (Node (Node (Node (Node (Leaf 16) (Leaf 16)) (Leaf 8)) (Leaf 4)) (Leaf 2)) (Leaf 6)) (Leaf 4), [])
    split (Node (Node (Node (Node (Node (Leaf 9) (Leaf 8)) (Leaf 1)) (Leaf 2)) (Leaf 15)) (Leaf 4), []) `shouldBe` Just (Node (Node (Node (Node (Node (Leaf 9) (Leaf 8)) (Leaf 1)) (Leaf 2)) (Node (Leaf 7) (Leaf 8))) (Leaf 4), [])
    split (Node (Node (Node (Node (Node (Leaf 9) (Leaf 8)) (Leaf 1)) (Leaf 2)) (Leaf 13)) (Leaf 4), []) `shouldBe` Just (Node (Node (Node (Node (Node (Leaf 9) (Leaf 8)) (Leaf 1)) (Leaf 2)) (Node (Leaf 6) (Leaf 7))) (Leaf 4), [])
  
  it "should calculate magnitude" $ do
    magnitude (Node (Node (Leaf 1) (Leaf 2)) (Node (Node (Leaf 3) (Leaf 4)) (Leaf 5))) `shouldBe` 143
    magnitude (Node (Node (Node (Node (Leaf 8) (Leaf 7)) (Node (Leaf 7) (Leaf 7))) (Node (Node (Leaf 8) (Leaf 6)) (Node (Leaf 7) (Leaf 7)))) (Node (Node (Node (Leaf 0) (Leaf 7)) (Node (Leaf 6) (Leaf 6))) (Node (Leaf 8) (Leaf 7)))) `shouldBe` 3488
