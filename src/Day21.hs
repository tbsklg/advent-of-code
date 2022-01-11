module Day21 where

import qualified Data.Map as M

data Player = Player {pos :: Int, space :: Int, score :: Int} deriving (Show, Eq, Ord)

solve :: [[Char]] -> Int
solve = resultOfSubgame . extract

solvePartTwo :: [[Char]] -> Int
solvePartTwo = fst . wins . extract

extract :: [[Char]] -> [Player]
extract raw = [player1, player2]
  where
    player1 = Player {pos = 1, space = read [last . head $ raw] :: Int, score = 0}
    player2 = Player {pos = 2, space = read [last . last $ raw] :: Int, score = 0}

resultOfSubgame :: [Player] -> Int
resultOfSubgame players = losingPoints * rolls
  where
    match = takeWhile score1000 . move $ players
    losingPoints = score . last $ match
    rolls = (+) 3 . (*) 3 . length $ match

score1000 :: Player -> Bool
score1000 Player {score = score} = score /= 1000

frequencies :: [(Int, Int)]
frequencies = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

wins :: [Player] -> (Int, Int)
wins players = winsUntil players 21

winsUntil :: [Player] -> Int -> (Int, Int)
winsUntil players score = snd . winsInUniverses spaceP1 spaceP2 0 0 $ M.empty
  where
    spaceP1 = space . head $ players
    spaceP2 = space . last $ players

    winsInUniverses spaceP1 spaceP2 scoreP1 scoreP2 state
      | scoreP1 >= score = (state, (1, 0))
      | scoreP2 >= score = (state, (0, 1))
      | otherwise = case M.lookup (spaceP1, spaceP2, scoreP1, scoreP2) state of
        Just a -> (state, a)
        Nothing -> (nextState, snd resultOfSubgame)
          where
            resultOfSubgame =
              foldl
                ( \(s, (spaceP1, spaceP2)) (rolls, times) ->
                    (fst (winsUntil rolls s), (spaceP1 + r1 rolls times s, spaceP2 + r2 rolls times s))
                )
                (state, (0, 0))
                frequencies

            r1 rolls times s = snd (snd (winsUntil rolls s)) * times
            r2 rolls times s = fst (snd (winsUntil rolls s)) * times
            winsUntil rolls s = winsInUniverses spaceP2 (nextSpace spaceP1 rolls) scoreP2 (scoreP1 + nextSpace spaceP1 rolls) s
            
            nextState = M.insert (spaceP1, spaceP2, scoreP1, scoreP2) (snd resultOfSubgame) (fst resultOfSubgame)

move :: [Player] -> [Player]
move players = move' players $ cycle [1 .. 100]
  where
    move' [] _ = []
    move' (Player {pos = p, space = space, score = sc} : xs) dice = turn : move' (xs ++ [turn]) nextDice
      where
        rolls = sum . take 3 $ dice
        nextSpace' = nextSpace space rolls
        nextScore = sc + nextSpace'

        turn = Player {pos = p, space = nextSpace', score = nextScore}
        nextDice = drop 3 dice

nextSpace :: Int -> Int -> Int
nextSpace space rolls
  | (space + rolls) `mod` 10 == 0 = 10
  | (space + rolls) > 10 = (space + rolls) `mod` 10
  | otherwise = space + rolls
