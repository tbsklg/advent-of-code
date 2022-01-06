module Day21 where

data Player = Player {pos:: Int, start :: Int, score :: Int} deriving (Show, Eq, Ord)

extract :: [[Char]] -> [Player]
extract raw = [player1, player2]
  where
    player1 = Player {pos= 1, start = read [last . head $ raw] :: Int, score = 0}
    player2 = Player {pos= 2, start = read [last . last $ raw] :: Int, score = 0}
