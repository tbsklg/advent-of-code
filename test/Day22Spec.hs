module Day22Spec where

import qualified Data.Map as M
import Day22 (Cube (..), Step (..), cardinality, extract, isOverlapping, onCubes, overlap, overlaps, reboot, step)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should extract steps" $ do
    extract ["on x=-24..25,y=-36..8,z=-15..31", "on x=-34..67,y=-1..2,z=-23..78"]
      `shouldBe` [Step {isOn = True, cube = Cube {x = (-24, 25), y = (-36, 8), z = (-15, 31)}}, Step {isOn = True, cube = Cube {x = (-34, 67), y = (-1, 2), z = (-23, 78)}}]

  it "should extract step" $ do
    step "on x=-24..25,y=-36..8,z=-15..31" `shouldBe` (Step {isOn = True, cube = Cube {x = (-24, 25), y = (-36, 8), z = (-15, 31)}})

  it "should check overlaps for two ranges" $ do
    isOverlapping (1, 2) (3, 4) `shouldBe` False
    isOverlapping (5, 9) (7, 8) `shouldBe` True
    isOverlapping (5, 9) (4, 6) `shouldBe` True
    isOverlapping (5, 9) (4, 5) `shouldBe` True
    isOverlapping (5, 9) (4, 4) `shouldBe` False
    isOverlapping (9, 11) (10, 12) `shouldBe` True

  it "should calculate number of elements for single step" $ do
    cardinality (Cube {x = (1, 2), y = (1, 2), z = (1, 2)}) `shouldBe` 8
    cardinality (Cube {x = (-1, -2), y = (-1, -2), z = (-1, -2)}) `shouldBe` 8
    cardinality (Cube {x = (1, 2), y = (2, 3), z = (3, 4)}) `shouldBe` 8
    cardinality (Cube {x = (1, 3), y = (2, 4), z = (3, 5)}) `shouldBe` 27
    cardinality (Cube {x = (-49, -5), y = (-3, 45), z = (-29, 18)}) `shouldBe` 105840

  it "should calculate cubes for intersection" $ do
    overlap (Cube {x = (9, 11), y = (9, 11), z = (9, 11)}) (Cube {x = (10, 12), y = (10, 12), z = (10, 12)})
      `shouldBe` Just (Cube {x = (10, 11), y = (10, 11), z = (10, 11)})

    overlap (Cube {x = (10, 12), y = (10, 12), z = (10, 12)}) (Cube {x = (9, 11), y = (9, 11), z = (9, 11)})
      `shouldBe` Just (Cube {x = (10, 11), y = (10, 11), z = (10, 11)})

  it "should calculate overlaps" $ do
    overlaps (Cube {x = (10, 12), y = (10, 12), z = (10, 12)}) M.empty
      `shouldBe` M.empty

    overlaps (Cube {x = (11, 12), y = (11, 12), z = (11, 12)}) (M.fromList [(Cube {x = (10, 12), y = (10, 12), z = (10, 12)}, 1)])
      `shouldBe` M.fromList
        [ ((Cube {x = (10, 12), y = (10, 12), z = (10, 12)}), 1),
          ((Cube {x = (11, 12), y = (11, 12), z = (11, 12)}), -1)
        ]

    overlaps
      (Cube {x = (11, 13), y = (11, 13), z = (11, 13)})
      ( M.fromList
          [ ((Cube {x = (11, 12), y = (11, 12), z = (11, 12)}), 0),
            ((Cube {x = (10, 12), y = (10, 12), z = (10, 12)}), 1)
          ]
      )
      `shouldBe` M.fromList
        [ ((Cube {x = (10, 12), y = (10, 12), z = (10, 12)}), 1),
          ((Cube {x = (11, 12), y = (11, 12), z = (11, 12)}), -1)
        ]
