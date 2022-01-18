module Day23Spec where

import Day23 (Amphipod (..), Burrow (..), Energy (..), Position (..), Room (..), Species (..), bla, nextState, consumed, getAllAmphipods, getFirstsInRoom, getLastsInRoom, hallway, move, roomFor)
import Test.Hspec (Spec, it, shouldBe, shouldContain, shouldMatchList)

spec :: Spec
spec = do
  it "should return current occupancy of space for species" $ do
    roomFor A targetBurrow `shouldBe` Room {for = A, amphipods = [Amphipod {species = A, position = (2, 3)}, Amphipod {species = A, position = (3, 3)}]}
    roomFor B targetBurrow `shouldBe` Room {for = B, amphipods = [Amphipod {species = B, position = (2, 5)}, Amphipod {species = B, position = (3, 5)}]}
    roomFor C targetBurrow `shouldBe` Room {for = C, amphipods = [Amphipod {species = C, position = (2, 7)}, Amphipod {species = C, position = (3, 7)}]}
    roomFor D targetBurrow `shouldBe` Room {for = D, amphipods = [Amphipod {species = D, position = (2, 9)}, Amphipod {species = D, position = (3, 9)}]}

  it "should find first amphipods in room" $ do
    getFirstsInRoom targetBurrow
      `shouldBe` [ Amphipod {species = A, position = (2, 3)},
                   Amphipod {species = B, position = (2, 5)},
                   Amphipod {species = C, position = (2, 7)},
                   Amphipod {species = D, position = (2, 9)}
                 ]

    getFirstsInRoom
      [ "#############",
        "#.........D.#",
        "###A#.#C#.###",
        "  #A#B#C#D#  ",
        "  #########  "
      ]
      `shouldBe` [ Amphipod {species = A, position = (2, 3)},
                   Amphipod {species = C, position = (2, 7)}
                 ]

  it "should find last amphipods in room" $ do
    getLastsInRoom
      [ "#############",
        "#AA.......D.#",
        "###.#.#C#.###",
        "  #.#B#C#D#  ",
        "  #########  "
      ]
      `shouldBe` [ Amphipod {species = B, position = (3, 5)},
                   Amphipod {species = C, position = (3, 7)},
                   Amphipod {species = D, position = (3, 9)}
                 ]

    getLastsInRoom
      [ "#############",
        "#AA.C.BCD.D.#",
        "###.#.#.#.###",
        "  #.#.#.#.#  ",
        "  #########  "
      ]
      `shouldBe` []

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
    bla
      [ "#############",
        "#...........#",
        "###B#C#B#D###",
        "  #A#D#C#A#  ",
        "  #########  "
      ]
      `shouldMatchList` [ ( [ "#############",
                              "#.B.........#",
                              "###.#C#B#D###",
                              "  #A#D#C#A#  ",
                              "  #########  "
                            ],
                            20
                          ),
                          ( [ "#############",
                              "#B..........#",
                              "###.#C#B#D###",
                              "  #A#D#C#A#  ",
                              "  #########  "
                            ],
                            30
                          ),
                          ( [ "#############",
                              "#...B.......#",
                              "###.#C#B#D###",
                              "  #A#D#C#A#  ",
                              "  #########  "
                            ],
                            20
                          ),
                          ( [ "#############",
                              "#.....B.....#",
                              "###.#C#B#D###",
                              "  #A#D#C#A#  ",
                              "  #########  "
                            ],
                            40
                          ),
                          ( [ "#############",
                              "#.......B...#",
                              "###.#C#B#D###",
                              "  #A#D#C#A#  ",
                              "  #########  "
                            ],
                            60
                          ),
                          ( [ "#############",
                              "#.........B.#",
                              "###.#C#B#D###",
                              "  #A#D#C#A#  ",
                              "  #########  "
                            ],
                            80
                          ),
                          ( [ "#############",
                              "#..........B#",
                              "###.#C#B#D###",
                              "  #A#D#C#A#  ",
                              "  #########  "
                            ],
                            90
                          )
                        ]

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

targetBurrow =
  [ "#############",
    "#...........#",
    "###A#B#C#D###",
    "  #A#B#C#D#  ",
    "  #########  "
  ]
