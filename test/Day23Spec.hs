module Day23Spec where

import Day23 (Amphipod (..), Burrow (..), Energy (..), Position (..), Room (..), Species (..), availableHallwayPositions, availableRoomPosition, canReachPositionFromHallway, canReachPositionFromRoom, consumed, getAllAmphipods, hallway, isInHallway, move, nextState, possiblePositions, simulate)
import Test.Hspec (Spec, it, shouldBe, shouldContain, shouldMatchList)

spec :: Spec
spec = do
  it "should find amphipods in hallway" $ do
    hallway
      [ "#############",
        "#AA.C.BCD.DB#",
        "###.#.#.#.###",
        "  #.#.#.#.#  ",
        "  #########  "
      ]
      `shouldBe` [ Amphipod {species = A, position = (1, 1)},
                   Amphipod {species = A, position = (1, 2)},
                   Amphipod {species = C, position = (1, 4)},
                   Amphipod {species = B, position = (1, 6)},
                   Amphipod {species = C, position = (1, 7)},
                   Amphipod {species = D, position = (1, 8)},
                   Amphipod {species = D, position = (1, 10)},
                   Amphipod {species = B, position = (1, 11)}
                 ]

  it "should simulate all possible states after initaliziation" $ do
    length
      ( simulate
          ( [ "#############",
              "#...........#",
              "###B#C#B#D###",
              "  #A#D#C#A#  ",
              "  #########  "
            ],
            0
          )
      )
      `shouldBe` 28

  it "should move an amphipod" $ do
    move
      [ "#############",
        "#...........#",
        "###B#C#B#D###",
        "  #A#D#C#A#  ",
        "  #########  "
      ]
      Amphipod {species = B, position = (2, 3)}
      (1, 1)
      `shouldBe` [ "#############",
                   "#B..........#",
                   "###.#C#B#D###",
                   "  #A#D#C#A#  ",
                   "  #########  "
                 ]

  it "should find all amphipods for burrow" $ do
    getAllAmphipods targetBurrow
      `shouldBe` [ Amphipod {species = A, position = (2, 3)},
                   Amphipod {species = B, position = (2, 5)},
                   Amphipod {species = C, position = (2, 7)},
                   Amphipod {species = D, position = (2, 9)},
                   Amphipod {species = A, position = (3, 3)},
                   Amphipod {species = B, position = (3, 5)},
                   Amphipod {species = C, position = (3, 7)},
                   Amphipod {species = D, position = (3, 9)}
                 ]

  it "should calculate needed energy" $ do
    consumed
      Amphipod {species = B, position = (2, 3)}
      (1, 1)
      `shouldBe` 30

    consumed
      Amphipod {species = A, position = (2, 3)}
      (1, 1)
      `shouldBe` 3

    consumed
      Amphipod {species = B, position = (2, 3)}
      (1, 11)
      `shouldBe` 90

    consumed
      Amphipod {species = D, position = (2, 3)}
      (1, 11)
      `shouldBe` 9000

  it "should calculate next state" $ do
    nextState
      ( [ "#############",
          "#...........#",
          "###B#C#B#D###",
          "  #A#D#C#A#  ",
          "  #########  "
        ],
        0
      )
      Amphipod {species = B, position = (2, 3)}
      (1, 1)
      `shouldBe` ( [ "#############",
                     "#B..........#",
                     "###.#C#B#D###",
                     "  #A#D#C#A#  ",
                     "  #########  "
                   ],
                   30
                 )
    nextState
      ( [ "#############",
          "#...........#",
          "###B#C#B#D###",
          "  #A#D#C#A#  ",
          "  #########  "
        ],
        130
      )
      Amphipod {species = B, position = (2, 3)}
      (1, 11)
      `shouldBe` ( [ "#############",
                     "#..........B#",
                     "###.#C#B#D###",
                     "  #A#D#C#A#  ",
                     "  #########  "
                   ],
                   220
                 )

  it "should return available room positions for amphipod" $ do
    availableRoomPosition
      [ "#############",
        "#...........#",
        "###B#A#C#D###",
        "  #B#A#C#D#  ",
        "  #########  "
      ]
      A
      `shouldBe` Nothing

    availableRoomPosition
      [ "#############",
        "#B..........#",
        "###.#A#C#D###",
        "  #B#A#C#D#  ",
        "  #########  "
      ]
      A
      `shouldBe` Nothing

    availableRoomPosition
      [ "#############",
        "#B.........B#",
        "###.#A#C#D###",
        "  #.#A#C#D#  ",
        "  #########  "
      ]
      A
      `shouldBe` Just (3, 3)

    availableRoomPosition
      [ "#############",
        "#B..........#",
        "###.#A#C#D###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      A
      `shouldBe` Just (2, 3)

  it "should check if amphipod can reach a position" $ do
    canReachPositionFromHallway
      [ "#############",
        "#B..A.......#",
        "###.#.#C#D###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      Amphipod {species = B, position = (1, 1)}
      (2, 5)
      `shouldBe` False

    canReachPositionFromHallway
      [ "#############",
        "#B....A.....#",
        "###.#.#C#D###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      Amphipod {species = B, position = (1, 1)}
      (2, 5)
      `shouldBe` True

    canReachPositionFromHallway
      [ "#############",
        "#...B......A#",
        "###.#.#C#D###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      Amphipod {species = A, position = (1, 11)}
      (2, 3)
      `shouldBe` False

    canReachPositionFromHallway
      [ "#############",
        "#.....A....B#",
        "###.#.#C#D###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      Amphipod {species = B, position = (1, 6)}
      (2, 3)
      `shouldBe` True

    canReachPositionFromRoom
      [ "#############",
        "#...........#",
        "###D#B#C#A###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      Amphipod {species = D, position = (2, 3)}
      (1, 11)
      `shouldBe` True

    canReachPositionFromRoom
      [ "#############",
        "#...........#",
        "###D#B#C#A###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      Amphipod {species = D, position = (3, 3)}
      (1, 1)
      `shouldBe` False

  it "should find free hallway positions" $ do
    availableHallwayPositions
      [ "#############",
        "#.....A....B#",
        "###.#.#C#D###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      `shouldBe` [(1, 1), (1, 2), (1, 4), (1, 8), (1, 10)]

    availableHallwayPositions
      [ "#############",
        "#.D...A....B#",
        "###.#.#C#D###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      `shouldBe` [(1, 1), (1, 4), (1, 8), (1, 10)]

  it "should check if amphipod is in hallway" $ do
    isInHallway
      [ "#############",
        "#.D...A....B#",
        "###.#.#C#D###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      Amphipod {species = A, position = (1, 6)}
      `shouldBe` True

    isInHallway
      [ "#############",
        "#.D...A....B#",
        "###.#.#C#D###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      Amphipod {species = C, position = (3, 7)}
      `shouldBe` False

  it "should calculate possible positions" $ do
    possiblePositions
      [ "#############",
        "#.D...A....B#",
        "###.#.#C#D###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      Amphipod {species = A, position = (1, 6)}
      `shouldBe` [(2, 3)]

    possiblePositions
      [ "#############",
        "#.D...D....B#",
        "###.#.#C#A###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      Amphipod {species = A, position = (2, 9)}
      `shouldBe` [(1, 8), (1, 10)]

    possiblePositions
      [ "#############",
        "#.D...D....B#",
        "###.#.#C#A###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      Amphipod {species = D, position = (1, 6)}
      `shouldBe` []

    possiblePositions
      [ "#############",
        "#...........#",
        "###D#B#C#A###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      Amphipod {species = D, position = (2, 3)}
      `shouldBe` [(1, 1), (1, 2), (1, 4), (1, 6), (1, 8), (1, 10), (1, 11)]

    possiblePositions
      [ "#############",
        "#...........#",
        "###D#B#C#A###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      Amphipod {species = A, position = (3, 3)}
      `shouldBe` []

targetBurrow :: [[Char]]
targetBurrow =
  [ "#############",
    "#...........#",
    "###A#B#C#D###",
    "  #A#B#C#D#  ",
    "  #########  "
  ]
