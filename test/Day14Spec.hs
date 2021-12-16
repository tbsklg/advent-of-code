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

  it "should count occurences from template" $ do
    frequencies "NNCB" `shouldBe` [('B', 1), ('C', 1), ('N', 2)]

  it "should count rules from template" $ do
    getRulesFromTemplate "NNCB" `shouldBe` [("CB", 1), ("NC", 1), ("NN", 1)]

  it "should number of characters from given rule" $ do
    countCharsFromRules rules [("CN", 2), ("NC", 1)] `shouldBe` [('B', 1), ('C', 2)]
    countCharsFromRules rules [("BC", 1), ("CH", 1), ("CN", 1), ("HB", 1), ("NB", 1), ("NC", 1)] `shouldBe` [('B', 4), ('C', 2)]

  it "should execute rules from rules count" $ do
    executeRules rules [("CN", 2), ("NC", 1)] `shouldBe` [("CCN", 2), ("NBC", 1)]
    executeRules rules [("CN", 2), ("NC", 2)] `shouldBe` [("CCN", 2), ("NBC", 2)]
    executeRules rules [("CN", 2432)] `shouldBe` [("CCN", 2432)]

  it "should execute rule for given rule key" $ do
    executeRule rules ("NN", 2) `shouldBe` ("NCN", 2)

  it "should extract rules for template" $ do
    extractRules [("CCN", 2), ("NBC", 1)] `shouldBe` [("BC", 1), ("CC", 2), ("CN", 2), ("NB", 1)]
    extractRules [("NCN", 3), ("NBC", 1), ("CHB", 1)] `shouldBe` [("BC", 1), ("CH", 1), ("CN", 3), ("HB", 1), ("NB", 1), ("NC", 3)]
    extractRules [("CCN", 2432)] `shouldBe` [("CC", 2432), ("CN", 2432)]

  it "should execuate polymerization for n times" $ do
    polymer rules "NNCB" 1 `shouldBe` [('B', 2), ('C', 2), ('H', 1), ('N', 2)]
    polymer rules "NNCB" 10 `shouldBe` [('B', 1749), ('C', 298), ('H', 161), ('N', 865)]

rules :: [([Char], [Char])]
rules = [("BB", "N"), ("BC", "B"), ("BH", "H"), ("BN", "B"), ("CB", "H"), ("CC", "N"), ("CH", "B"), ("CN", "C"), ("HB", "C"), ("HC", "B"), ("HH", "N"), ("HN", "C"), ("NB", "B"), ("NC", "B"), ("NH", "C"), ("NN", "C")]
