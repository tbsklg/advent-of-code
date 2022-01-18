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

roomPositions :: Species -> [Position]
roomPositions A = [(2, 3), (3, 3)]
roomPositions B = [(2, 5), (3, 5)]
roomPositions C = [(2, 7), (3, 7)]
roomPositions D = [(2, 9), (3, 9)]

canReachPosition :: Burrow -> Position -> Position -> Bool
canReachPosition burrow from to = canMoveHorizontally && canMoveVertically
  where
    canMoveHorizontally = (==) 0 . length . mapMaybe (getAmphipod burrow) $ horizontally
    canMoveVertically = (==) 0 . length . mapMaybe (getAmphipod burrow) $ vertically

    moveLeft = snd to < snd from

    horizontally
      | moveLeft = [(x, y) | x <- [fst from], y <- [(snd to) .. snd from - 1]]
      | otherwise = [(x, y) | x <- [fst from], y <- [(snd from + 1) .. (snd to)]]

    vertically = [(x, y) | x <- [fst from + 1 .. fst to], y <- [snd to]]

availableRoomPosition :: Burrow -> Species -> Maybe Position
availableRoomPosition burrow for
  | null amphipodsInRoom = Just . last $ positionsInRoom
  | onlyOneAmphipod && sameSpecies = Just . head $ positionsInRoom
  | otherwise = Nothing
  where
    sameSpecies = (==) for . species . head $ amphipodsInRoom
    onlyOneAmphipod = length amphipodsInRoom == 1
    positionsInRoom = roomPositions for
    amphipodsInRoom = mapMaybe (getAmphipod burrow) positionsInRoom

emptySpace :: Char
emptySpace = '.'

bla :: Burrow -> [(Burrow, Int)]
bla burrow = simulate initialState initials hallwayPositions
  where
    simulate state [] (_ : _) = []
    simulate state (_ : xs) [] = simulate state xs hallwayPositions
    simulate state (amphipod : as) (position : ps) = nextState state amphipod position : simulate state (amphipod : as) ps
    simulate _ _ _ = error "Error occured while simulation!"

    initials = getFirstsInRoom burrow
    initialState = (burrow, 0)

-- possiblePositions :: Burrow -> Amphipod -> [Position]
-- possiblePositions burrow amphipd =

--   where
--     freeHallwayPositions = filter (\ position -> position /= (position amphipod)) hallwayPositions
--     homeRoomPositions = roomPositions

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

hallwayPositions :: [Position]
hallwayPositions = [(1, 1), (1, 2), (1, 4), (1, 6), (1, 8), (1, 10), (1, 11)]

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