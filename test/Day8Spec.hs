module Day8Spec where

import Day8 (Entry (..), Output (..), Signal (..), extractEntry, matching, solve, uniques)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should solve for part one" $ do
    solve
      [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
        "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
        "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
        "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
        "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
        "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
        "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
        "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
        "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
        "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
      ]
      `shouldBe` 26

  it "should extract signal and from line" $ do
    extractEntry "fdceba bafdgc abeg afbdgec | bafdec cgefd "
      `shouldBe` Entry (Signal ["fdceba", "bafdgc", "abeg", "afbdgec"]) (Output ["bafdec", "cgefd"])

  it "should filter unique signals" $ do
    uniques ["be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"]
      `shouldBe` ["be", "cfbegad", "cgeb", "edb"]

  it "should extract unique signals from output" $ do
    matching (Entry (Signal ["be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"]) (Output ["fdgacbe", "cefdb", "cefbgd", "gcbe"]))
      `shouldBe` ["fdgacbe", "gcbe"]
