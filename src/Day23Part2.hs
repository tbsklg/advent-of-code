module Day23Part2 where

import Data.Function (on)
import Data.List (minimumBy, sortBy, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Semigroup (All (getAll))

import qualified Data.Map as M
import qualified Data.Set as S

type Burrow = [String]

type Energy = Int

type Hallway = [Amphipod]

type Position = (Int, Int)

type State = (Burrow, Int)

data Species = A | B | C | D deriving (Show, Eq, Ord)

data Room = Room {for :: Species, amphipods :: [Amphipod]} deriving (Show, Eq, Ord)

data Amphipod = Amphipod {species :: Species, position :: Position} deriving (Show, Eq, Ord)

solve :: [[Char]] -> Int
solve = findMinimum

targetPartTwo :: Burrow
targetPartTwo =
  [ "#############",
    "#...........#",
    "###A#B#C#D###",
    "  #A#B#C#D#  ",
    "  #A#B#C#D#  ",
    "  #A#B#C#D#  ",
    "  #########  "
  ]

emptySpace :: Char
emptySpace = '.'

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
roomPositions A = [(2, 3), (3, 3), (4, 3), (5, 3)]
roomPositions B = [(2, 5), (3, 5), (4, 5), (5, 5)]
roomPositions C = [(2, 7), (3, 7), (4, 7), (5, 7)]
roomPositions D = [(2, 9), (3, 9), (4, 9), (5, 9)]

hallway :: Burrow -> [Amphipod]
hallway burrow = mapMaybe (getAmphipod burrow) . zip [1, 1 ..] $ [1 .. 11]

hallwayPositions :: [Position]
hallwayPositions = [(1, 1), (1, 2), (1, 4), (1, 6), (1, 8), (1, 10), (1, 11)]

findMinimum :: [String] -> Int
findMinimum burrow = findMinimum' initialState
  where
    findMinimum' state = case M.lookup targetPartTwo state of
      Just a -> a
      Nothing -> findMinimum' nextState'
      where
        nextState' = M.fromListWith (\x y -> minimum [x, y]) . concatMap simulate . M.toList $ state
    initialState = M.fromList [(burrow, 0)]

simulate :: State -> [State]
simulate currentState = M.toList . M.fromList . concat . simulate' $ amphipods
  where
    simulate' [] = []
    simulate' (x : xs) = nextStates x (possiblePositions currentBurrow x) : simulate' xs

    nextStates _ [] = []
    nextStates amphipod (x : xs) = nextState currentState amphipod x : nextStates amphipod xs

    amphipods = getAllAmphipods currentBurrow
    currentBurrow = fst currentState

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

possiblePositions :: Burrow -> Amphipod -> [Position]
possiblePositions burrow amphipod
  | isInTarget burrow amphipod = []
  | isInHallway burrow amphipod = reachableRoomPositions
  | otherwise = reachableHallwayPositions
  where
    reachableHallwayPositions =
      filter (canReachPositionFromRoom burrow amphipod)
        . availableHallwayPositions
        $ burrow

    reachableRoomPositions = case availableRoomPosition burrow (species amphipod) of
      Just a -> [a | canReachPositionFromHallway burrow amphipod a]
      Nothing -> []

isInTarget :: Burrow -> Amphipod -> Bool
isInTarget burrow amphipod
  | isCorrectRoom && isOnLastPosition = True
  | otherwise = isCorrectRoom && isSameSpeciesOnDeeperPosition
  where
    isCorrectRoom = position amphipod `elem` roomPositions (species amphipod)
    isOnLastPosition = position amphipod == (last . roomPositions . species $ amphipod)

    isSameSpeciesOnDeeperPosition =
      foldl
        ( \x y ->
            x && case getAmphipod burrow y of
              Just a -> (species a == species amphipod) && (fst . position $ a) > (fst . position $ amphipod)
              Nothing -> False
        )
        True
        . roomPositions
        . species
        $ amphipod

canReachPositionFromHallway :: Burrow -> Amphipod -> Position -> Bool
canReachPositionFromHallway burrow amphipod to = canMoveHorizontally && canMoveVertically
  where
    from = position amphipod

    canMoveHorizontally = (==) 0 . length . mapMaybe (getAmphipod burrow) $ horizontally
    canMoveVertically = (==) 0 . length . mapMaybe (getAmphipod burrow) $ vertically

    moveLeft = snd to < snd from

    horizontally
      | moveLeft = [(x, y) | x <- [fst from], y <- [(snd to) .. snd from - 1]]
      | otherwise = [(x, y) | x <- [fst from], y <- [(snd from + 1) .. (snd to)]]

    vertically = [(x, y) | x <- [fst from + 1 .. fst to], y <- [snd to]]

canReachPositionFromRoom :: Burrow -> Amphipod -> Position -> Bool
canReachPositionFromRoom burrow amphipod to = canMoveHorizontally && canMoveVertically
  where
    from = position amphipod

    canMoveHorizontally = (==) 0 . length . mapMaybe (getAmphipod burrow) $ horizontally
    canMoveVertically = (==) 0 . length . mapMaybe (getAmphipod burrow) $ vertically

    moveLeft = snd to < snd from

    horizontally
      | moveLeft = [(x, y) | x <- [1], y <- [(snd to) .. snd from - 1]]
      | otherwise = [(x, y) | x <- [1], y <- [(snd from + 1) .. (snd to)]]

    vertically = [(x, y) | x <- [fst to .. fst from - 1], y <- [snd from]]

availableHallwayPositions :: Burrow -> [Position]
availableHallwayPositions burrow = findPositions hallwayPositions
  where
    findPositions [] = []
    findPositions (x : xs) = case getAmphipod burrow x of
      Just _ -> findPositions xs
      _ -> x : findPositions xs

availableRoomPosition :: Burrow -> Species -> Maybe Position
availableRoomPosition burrow for
  | null amphipodsInRoom = Just . last $ positionsInRoom
  | sameSpecies = Just nextPosition
  | otherwise = Nothing
  where
    sameSpecies = all (\x -> species x == for) amphipodsInRoom
    nextPosition = ((minimum . map (fst . position) $ amphipodsInRoom) - 1, snd . head $ positionsInRoom)

    positionsInRoom = roomPositions for
    amphipodsInRoom = mapMaybe (getAmphipod burrow) positionsInRoom

isInHallway :: Burrow -> Amphipod -> Bool
isInHallway burrow amphipod = isInHallway hallwayPositions
  where
    isInHallway [] = False
    isInHallway (x : xs) = position amphipod == x || isInHallway xs

getAmphipod :: Burrow -> Position -> Maybe Amphipod
getAmphipod burrow position
  | c `elem` ['A', 'B', 'C', 'D'] = Just Amphipod {species = speciesFrom c, position = position}
  | otherwise = Nothing
  where
    c = burrow !! fst position !! snd position

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