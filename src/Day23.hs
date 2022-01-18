module Day23 where

import Control.Arrow (Arrow (second))
import Data.Function
import Data.List
import Data.Maybe (catMaybes, mapMaybe)
import Data.Semigroup (All (getAll))
import Debug.Trace (traceShow)

type Burrow = [String]

type Energy = Int

type Hallway = [Amphipod]

type Position = (Int, Int)

type State = (Burrow, Int)

data Species = A | B | C | D deriving (Show, Eq, Ord)

data Room = Room {for :: Species, amphipods :: [Amphipod]} deriving (Show, Eq, Ord)

data Amphipod = Amphipod {species :: Species, position :: Position} deriving (Show, Eq, Ord)


target :: Burrow
target =
  [ "#############",
    "#...........#",
    "###A#B#C#D###",
    "  #A#B#C#D#  ",
    "  #########  "
  ]

consumes :: Species -> Energy
consumes A = 1
consumes B = 10
consumes C = 100
consumes D = 1000

speciesFrom :: Char -> Species
speciesFrom 'A' = A
speciesFrom 'B' = B
speciesFrom 'C' = C
speciesFrom 'D' = D
speciesFrom _ = error "Unsupported species!"

speciesTo :: Species -> Char
speciesTo A = 'A'
speciesTo B = 'B'
speciesTo C = 'C'
speciesTo D = 'D'

amphipodFrom :: Species -> Position -> Amphipod
amphipodFrom s p = Amphipod {species = s, position = p}

roomFor :: Species -> Burrow -> Room
roomFor A b = Room {for = A, amphipods = amphipodFrom (speciesFrom (b !! 2 !! 3)) (2, 3) : [amphipodFrom (speciesFrom (b !! 3 !! 3)) (3, 3)]}
roomFor B b = Room {for = B, amphipods = amphipodFrom (speciesFrom (b !! 2 !! 5)) (2, 5) : [amphipodFrom (speciesFrom (b !! 3 !! 5)) (3, 5)]}
roomFor C b = Room {for = C, amphipods = amphipodFrom (speciesFrom (b !! 2 !! 7)) (2, 7) : [amphipodFrom (speciesFrom (b !! 3 !! 7)) (3, 7)]}
roomFor D b = Room {for = D, amphipods = amphipodFrom (speciesFrom (b !! 2 !! 9)) (2, 9) : [amphipodFrom (speciesFrom (b !! 3 !! 9)) (3, 9)]}

emptySpace :: Char
emptySpace = '.'

bla :: Burrow -> [(Burrow, Int)]
bla burrow = []

nextState :: State -> Amphipod -> Position -> State
nextState (burrow, energy) amphipod to = (nextBurrow, nextEnergy)
  where
    nextBurrow = move burrow amphipod to
    nextEnergy = (+) energy . consumed amphipod $ to

consumed :: Amphipod -> Position -> Int
consumed amphipod to = (*) distance . consumes . species $ amphipod
  where
    distance = rowDistance + columnDistance

    rowDistance = abs ((fst . position $ amphipod) - fst to)
    columnDistance = abs ((snd . position $ amphipod) - snd to)

move :: Burrow -> Amphipod -> Position -> Burrow
move burrow amphipod to = updatedBurrow
  where
    updatedBurrow = updateBurrow clearedBurrow rowIndex updatedRow

    updatedRow = updateRow rowToUpdate at (speciesTo . species $ amphipod)
    rowToUpdate = burrow !! rowIndex
    at = snd to
    rowIndex = fst to

    clearedBurrow = updateBurrow burrow (fst cellIndex) (updateRow clearedRow (snd cellIndex) emptySpace)
    clearedRow = burrow !! fst cellIndex
    cellIndex = position amphipod

updateBurrow :: Burrow -> Int -> String -> Burrow
updateBurrow burrow index row = updatedRows
  where
    updatedRows = map snd . sortBy (compare `on` fst) $ allRows
    allRows = rest ++ [newRow]
    newRow = (index, row)
    rest = filter (\(rowIndex, row) -> rowIndex /= index) . zip [0 ..] $ burrow

updateRow :: String -> Int -> Char -> String
updateRow row position update = before ++ [update] ++ after
  where
    before = take position row
    after = drop (position + 1) row

getAmphipod :: Burrow -> Position -> Maybe Amphipod
getAmphipod burrow position
  | c `elem` ['A', 'B', 'C', 'D'] = Just Amphipod {species = speciesFrom c, position = position}
  | otherwise = Nothing
  where
    c = burrow !! fst position !! snd position

getFirstsInRoom :: Burrow -> [Amphipod]
getFirstsInRoom burrow = catMaybes [firstRoom, secondRoom, thirdRoom, fourthRoom]
  where
    firstRoom = getAmphipod burrow (2, 3)
    secondRoom = getAmphipod burrow (2, 5)
    thirdRoom = getAmphipod burrow (2, 7)
    fourthRoom = getAmphipod burrow (2, 9)

getLastsInRoom :: Burrow -> [Amphipod]
getLastsInRoom burrow = catMaybes [firstRoom, secondRoom, thirdRoom, fourthRoom]
  where
    firstRoom = getAmphipod burrow (3, 3)
    secondRoom = getAmphipod burrow (3, 5)
    thirdRoom = getAmphipod burrow (3, 7)
    fourthRoom = getAmphipod burrow (3, 9)

hallway :: Burrow -> [Amphipod]
hallway burrow = mapMaybe (getAmphipod burrow) . zip [1, 1 ..] $ [1 .. 11]

getAllAmphipods :: Burrow -> [Amphipod]
getAllAmphipods burrow =
  catMaybes
    . concatMap
      ( \(rowIndex, row) ->
          zipWith
            ( \columnIndex column ->
                getAmphipod burrow (rowIndex, columnIndex)
            )
            [0 ..]
            row
      )
    . zip [0 ..]
    $ burrow