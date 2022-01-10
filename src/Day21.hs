module Day21 where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Debug.Trace (trace, traceShow)

data Player = Player {pos :: Int, space :: Int, score :: Int} deriving (Show, Eq, Ord)

solve :: [[Char]] -> Int
solve = result . extract

extract :: [[Char]] -> [Player]
extract raw = [player1, player2]
  where
    player1 = Player {pos = 1, space = read [last . head $ raw] :: Int, score = 0}
    player2 = Player {pos = 2, space = read [last . last $ raw] :: Int, score = 0}

result :: [Player] -> Int
result players = losingPoints * rolls
  where
    match = takeWhile score1000 . move $ players
    losingPoints = score . last $ match
    rolls = (+) 3 . (*) 3 . length $ match

score1000 :: Player -> Bool
score1000 Player {score = score} = score /= 1000

frequencies :: M.Map Int Int
frequencies = M.fromList [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

frequencies' :: [Int]
frequencies' = [x + y + z | x <- [1 .. 3], y <- [1 .. 3], z <- [1 .. 3]]

computeWins p1 p2 s1 s2 state
  | s1 >= 10 = (state, (1, 0))
  | s2 >= 10 = (state, (0, 1))
  | otherwise = case M.lookup (p1, p2, s1, s2) state of
    Just a -> traceShow state (state, a)
    Nothing -> (nextState, result)
      where
        result = foldl (\(p1, p2) rolls -> (p1 + (snd . snd . wins $ rolls), p2 + (fst . snd . wins $ rolls))) (0, 0) frequencies'
        wins rolls = computeWins p2 (nextSpace p1 rolls) s2 (s1 + nextSpace p1 rolls) state

        nextState = M.insert (p1, p2, s1, s2) result state

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
