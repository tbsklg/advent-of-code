module Day10Spec where

import Day10 (completeLine, firstIncorrect, score, scorePartTwo)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should calculate score" $ do
    score ["[({(<(())[]>[[{[]{<()<>>", "[(()[<>])]({[<{<<[]>>(", "{([(<{}[<>[]}>{[]{[(<()>", "(((({<>}<{<{<>}{[]{[]{}", "[[<[([]))<([[{}[[()]]]", "[{[{({}]{}}([{[{{{}}([]", "{<[[]]>}<{[{[{[]{()[[[]", "[<(<(<(<{}))><([]([]()", "<{([([[(<>()){}]>(<<{{", "<{([{{}}[<[[[<>{}]]]>[]]"] `shouldBe` 26397

  it "should calculate score for part two" $ do
    scorePartTwo ["[({(<(())[]>[[{[]{<()<>>", "[(()[<>])]({[<{<<[]>>(", "{([(<{}[<>[]}>{[]{[(<()>", "(((({<>}<{<{<>}{[]{[]{}", "[[<[([]))<([[{}[[()]]]", "[{[{({}]{}}([{[{{{}}([]", "{<[[]]>}<{[{[{[]{()[[[]", "[<(<(<(<{}))><([]([]()", "<{([([[(<>()){}]>(<<{{", "<{([{{}}[<[[[<>{}]]]>[]]"] `shouldBe` 288957

  it "should validate line" $ do
    firstIncorrect "{([(<{}[<>[]}>{[]{[(<()>" `shouldBe` "}"
    firstIncorrect "[[<[([]))<([[{}[[()]]]" `shouldBe` ")"
    firstIncorrect "[{[{({}]{}}([{[{{{}}([]" `shouldBe` "]"
    firstIncorrect "[<(<(<(<{}))><([]([]()" `shouldBe` ")"
    firstIncorrect "<{([([[(<>()){}]>(<<{{" `shouldBe` ">"

  it "should repair line" $ do
    completeLine "[({(<(())[]>[[{[]{<()<>>" `shouldBe` "}}]])})]"
    completeLine "[(()[<>])]({[<{<<[]>>(" `shouldBe` ")}>]})"
    completeLine "(((({<>}<{<{<>}{[]{[]{}" `shouldBe` "}}>}>))))"
    completeLine "{<[[]]>}<{[{[{[]{()[[[]" `shouldBe` "]]}}]}]}>"
    completeLine "<{([{{}}[<[[[<>{}]]]>[]]" `shouldBe` "])}>"
