module Day21 where

import Debug.Trace (trace, traceShow)

data Player = Player {pos :: Int, space :: Int, score :: Int, rolls :: [Int]} deriving (Show, Eq, Ord)

solve :: [[Char]] -> Int
solve = result . extract

extract :: [[Char]] -> [Player]
extract raw = [player1, player2]
  where
    player1 = Player {pos = 1, space = read [last . head $ raw] :: Int, score = 0, rolls = []}
    player2 = Player {pos = 2, space = read [last . last $ raw] :: Int, score = 0, rolls = []}

result :: [Player] -> Int
result players = losingPoints * rolls
    where
        match = takeWhile score1000 . move $ players
        losingPoints = score . last $ match
        rolls = (+) 3 . (*) 3 . length $ match

score1000 :: Player -> Bool
score1000 Player {score = score} = score /= 1000

move :: [Player] -> [Player]
move players = move' players $ cycle [1 .. 100]
  where
    move' [] _ = []
    move' (Player {pos = p, space = space, score = sc} : xs) dice = turn : move' (xs ++ [turn]) nextDice
      where
        rolls = sum . take 3 $ dice
        nextSpace' = nextSpace space rolls
        nextScore = sc + nextSpace'

        turn = Player {pos = p, space = nextSpace', score = nextScore, rolls = take 3 dice}
        nextDice = drop 3 dice

nextSpace :: Int -> Int -> Int
nextSpace space rolls
    | (space + rolls) `mod` 10 == 0 = 10
    | (space + rolls) > 10 = (space + rolls) `mod` 10
    | otherwise = space + rolls
