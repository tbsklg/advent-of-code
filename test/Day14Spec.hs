module Day14Spec where

import Day14 (countCharsFromRules, executeRule, executeRules, extract, extractRules, frequencies, getRulesFromTemplate, polymer, transform)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should extract raw data" $ do
    extract ["NNCB", "", "CH -> B", "HH -> N", "CB -> H", "NH -> C", "HB -> C", "HC -> B", "HN -> C", "NN -> C", "BH -> H", "NC -> B", "NB -> B", "BN -> B", "BB -> N", "BC -> B", "CC -> N", "CN -> C"]
      `shouldBe` ("NNCB", ["CH -> B", "HH -> N", "CB -> H", "NH -> C", "HB -> C", "HC -> B", "HN -> C", "NN -> C", "BH -> H", "NC -> B", "NB -> B", "BN -> B", "BB -> N", "BC -> B", "CC -> N", "CN -> C"])

  it "should transform data" $ do
    transform ("NNCB", ["CH -> B", "HH -> N", "CB -> H", "NH -> C", "HB -> C", "HC -> B", "HN -> C", "NN -> C", "BH -> H", "NC -> B", "NB -> B", "BN -> B", "BB -> N", "BC -> B", "CC -> N", "CN -> C"])
      `shouldBe` ("NNCB", [("BB", "N"), ("BC", "B"), ("BH", "H"), ("BN", "B"), ("CB", "H"), ("CC", "N"), ("CH", "B"), ("CN", "C"), ("HB", "C"), ("HC", "B"), ("HH", "N"), ("HN", "C"), ("NB", "B"), ("NC", "B"), ("NH", "C"), ("NN", "C")])

  it "should count occurences" $ do
    frequencies "NNCB" `shouldBe` [('B', 1), ('C', 1), ('N', 2)]

  it "should count rules for template" $ do
    getRulesFromTemplate "NNCB" `shouldBe` [("CB", 1), ("NC", 1), ("NN", 1)]

  it "should count occurences" $ do
    countCharsFromRules rules [("CN", 2), ("NC", 1)] `shouldBe` [('B', 1), ('C', 2)]
    countCharsFromRules rules [("BC", 1), ("CH", 1), ("CN", 1), ("HB", 1), ("NB", 1), ("NC", 1)] `shouldBe` [('B', 4), ('C', 2)]

  it "should execute rules" $ do
    executeRules rules [("CN", 2), ("NC", 1)] `shouldBe` ["CCN", "CCN", "NBC"]
    executeRules rules [("CN", 2), ("NC", 2)] `shouldBe` ["CCN", "CCN", "NBC", "NBC"]

  it "should execute rule for key" $ do
    executeRule rules ("NN", 2) `shouldBe` ["NCN", "NCN"]

  it "should extract rules for template" $ do
    extractRules ["NCN", "NCN"] `shouldBe` [("CN", 2), ("NC", 2)]
    extractRules ["NCN", "NBC", "CHB"] `shouldBe` [("BC", 1), ("CH", 1), ("CN", 1), ("HB", 1), ("NB", 1), ("NC", 1)]

  it "should polymer" $ do
    polymer rules "NNCB" 1 `shouldBe` [('B', 2), ('C', 2), ('H', 1), ('N', 2)]
    polymer rules "NNCB" 10 `shouldBe` [('B', 1749), ('C', 298), ('H', 161), ('N', 865)]

rules :: [([Char], [Char])]
rules = [("BB", "N"), ("BC", "B"), ("BH", "H"), ("BN", "B"), ("CB", "H"), ("CC", "N"), ("CH", "B"), ("CN", "C"), ("HB", "C"), ("HC", "B"), ("HH", "N"), ("HN", "C"), ("NB", "B"), ("NC", "B"), ("NH", "C"), ("NN", "C")]
