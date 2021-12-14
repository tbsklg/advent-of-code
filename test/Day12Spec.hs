module Day12Spec where

import Day12 (countPaths, countPathsPartTwo, extractPath, groupNeighbours)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should extract paths" $ do
    extractPath ["dc-end", "HN-start", "start-kj", "dc-start", "dc-HN", "LN-dc", "HN-end", "kj-sa", "kj-HN", "kj-dc"]
      `shouldBe` [("dc", "end"), ("HN", "start"), ("start", "kj"), ("dc", "start"), ("dc", "HN"), ("LN", "dc"), ("HN", "end"), ("kj", "sa"), ("kj", "HN"), ("kj", "dc")]
    extractPath ["start-A", "start-b", "A-c", "A-b", "b-d", "A-end", "b-end"] `shouldBe` [("start", "A"), ("start", "b"), ("A", "c"), ("A", "b"), ("b", "d"), ("A", "end"), ("b", "end")]

  it "should group by neighbours" $ do
    groupNeighbours [("dc", "end"), ("HN", "start"), ("start", "kj"), ("dc", "start"), ("dc", "HN"), ("LN", "dc"), ("HN", "end"), ("kj", "sa"), ("kj", "HN"), ("kj", "dc")]
      `shouldBe` [("HN", ["end", "kj", "dc"]), ("LN", ["dc"]), ("dc", ["HN", "end", "kj", "LN"]), ("kj", ["dc", "HN", "sa"]), ("sa", ["kj"]), ("start", ["kj", "dc", "HN"])]

    groupNeighbours [("start", "A"), ("start", "b"), ("A", "c"), ("A", "b"), ("b", "d"), ("A", "end"), ("b", "end")]
      `shouldBe` [("A", ["end", "b", "c"]), ("b", ["end", "d", "A"]), ("c", ["A"]), ("d", ["b"]), ("start", ["b", "A"])]

  it "should count paths" $ do
    countPaths [("start", ["A"]), ("A", ["end"])] `shouldBe` 1
    countPaths [("start", ["A", "b"]), ("A", ["end"]), ("b", ["end"])] `shouldBe` 2
    countPaths [("start", ["A", "b"]), ("A", ["end"]), ("b", ["end", "d"]), ("d", ["b"])] `shouldBe` 2
    countPaths [("start", ["A", "b"]), ("A", ["end", "c"]), ("b", ["end", "d"]), ("d", ["b"]), ("c", ["A"])] `shouldBe` 3
    countPaths [("start", ["A", "b"]), ("A", ["end", "c", "b"]), ("b", ["end", "d", "A"]), ("d", ["b"]), ("c", ["A"])] `shouldBe` 10

    countPaths [("HN", ["end", "kj", "dc"]), ("LN", ["dc"]), ("dc", ["HN", "end", "kj", "LN"]), ("kj", ["dc", "HN", "sa"]), ("sa", ["kj"]), ("start", ["kj", "dc", "HN"])] `shouldBe` 19

  it "should count paths" $ do
    countPathsPartTwo [("start", ["A"]), ("A", ["end"])] `shouldBe` 1
    countPathsPartTwo [("start", ["A", "b"]), ("A", ["end"]), ("b", ["end"])] `shouldBe` 2
    countPathsPartTwo [("start", ["A", "b"]), ("A", ["end"]), ("b", ["end", "d"]), ("d", ["b"])] `shouldBe` 3
    countPathsPartTwo [("start", ["A", "b"]), ("A", ["c", "end"]), ("b", ["d", "end"]), ("d", ["b"]), ("c", ["A"])] `shouldBe` 5
    countPathsPartTwo [("start", ["A", "b"]), ("A", ["end", "c", "b"]), ("b", ["end", "d", "A"]), ("d", ["b"]), ("c", ["A"])] `shouldBe` 36

    countPathsPartTwo [("HN", ["end", "kj", "dc"]), ("LN", ["dc"]), ("dc", ["HN", "end", "kj", "LN"]), ("kj", ["dc", "HN", "sa"]), ("sa", ["kj"]), ("start", ["kj", "dc", "HN"])] `shouldBe` 103
