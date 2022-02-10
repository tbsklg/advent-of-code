module Day18 where

import Control.Applicative ((<|>))
import Data.Char (isDigit, isNumber)
import Data.Maybe (fromJust)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq, Ord)

data Direction = R | L deriving (Show, Eq, Ord)

data Crumb a = LeftCrumb (Tree a) | RightCrumb (Tree a) deriving (Show, Eq, Ord)

type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

-- see http://learnyouahaskell.com/zippers

max' :: Ord p => p -> p -> p
max' a b = if a > b then a else b

maxDepth' :: Tree Int -> Int
maxDepth' (Node l r) = 1 + max' (maxDepth' l) (maxDepth' r)
maxDepth' (Leaf _) = 0

add :: Tree Int -> Tree Int -> Tree Int
add = Node

solve :: [[Char]] -> Int
solve = magnitude . performAdd

solvePartTwo :: [[Char]] -> Int
solvePartTwo = maximum . cumulative

cumulative :: [[Char]] -> [Int]
cumulative [] = []
cumulative lines = cumulative' lines : cumulative (tail lines)
  where
    cumulative' [] = 0
    cumulative' (x : xs) = foldl (\max next -> maximum [magnitude' x next, magnitude' next x, max]) 0 xs

    magnitude' x y = magnitude t
      where
        (t, _) = reduce (add (slrParse x) (slrParse y), [])

performAdd :: [[Char]] -> Tree Int
performAdd lines = t
  where
    (t, _) = foldl (\(x, _) y -> reduce (add x (slrParse y), [])) (slrParse (head lines), []) (tail lines)

reduce :: Zipper Int -> Zipper Int
reduce t@(Node _ _, []) = case explode t of
  Just t' -> reduce t'
  _ -> maybe (goTop t) reduce (split t)
reduce _ = error "Invalid state!"

magnitude :: Tree Int -> Int
magnitude (Leaf n) = n
magnitude (Node l r) = magnitude l * 3 + magnitude r * 2

explode :: Zipper Int -> Maybe (Zipper Int)
explode t@(Leaf _, _) = Nothing
explode t@(Node _ _, _) = case pairAtDepth 4 t of
  Just a -> Just . goTop $ result
  _ -> Nothing
  where
    t1@(Node (Leaf ll) (Leaf rl), _) = fromJust . pairAtDepth 4 $ t

    newL = case nearestLeft t1 of
      Just t'@(Leaf ll', _) -> attach (Leaf (ll' + ll)) t'
      _ -> t1

    t2@(_, _) = fromJust . pairAtDepth 4 . goTop $ newL
    newR = case nearestRight t2 of
      Just t'@(Leaf rl', _) -> attach (Leaf (rl' + rl)) t'
      _ -> newL

    t3@(_, _) = fromJust . pairAtDepth 4 . goTop $ newR
    result = attach (Leaf 0) t3

nearestLeft :: Zipper Int -> Maybe (Zipper Int)
nearestLeft t@(_, []) = Nothing
nearestLeft t@(_, LeftCrumb _ : _) = nearestLeft =<< goUp t
nearestLeft t@(_, RightCrumb _ : _) = rightNearest =<< goLeft =<< goUp t

rightNearest :: Zipper Int -> Maybe (Zipper Int)
rightNearest t@(Leaf _, _) = Just t
rightNearest t@(Node _ _, _) = rightNearest =<< goRight t

nearestRight :: Zipper Int -> Maybe (Zipper Int)
nearestRight t@(_, []) = Nothing
nearestRight t@(_, RightCrumb _ : _) = nearestRight =<< goUp t
nearestRight t@(_, LeftCrumb _ : _) = leftNearest =<< goRight =<< goUp t

leftNearest :: Zipper Int -> Maybe (Zipper Int)
leftNearest t@(Leaf _, _) = Just t
leftNearest t@(Node _ _, _) = leftNearest =<< goLeft t

split :: Zipper Int -> Maybe (Zipper Int)
split t = case splitAt' t of
  Just t'@(Leaf a, _) -> Just . goTop . attach nN $ t'
    where
      nN = Node (Leaf lP) (Leaf rP)
      lP = a `div` 2
      rP = ceiling (fromIntegral a / 2)
  Just t@(_, _) -> Nothing
  Nothing -> Nothing

splitAt' :: Zipper Int -> Maybe (Zipper Int)
splitAt' t@(Leaf a, _)
  | a >= 10 = Just t
  | otherwise = Nothing
splitAt' t@(Node _ _, _) = (splitAt' =<< goLeft t) <|> (splitAt' =<< goRight t)

pairAtDepth :: Int -> Zipper Int -> Maybe (Zipper Int)
pairAtDepth n (Leaf _, _) = Nothing
pairAtDepth 0 t@(_, _) = Just t
pairAtDepth n t@(Node _ _, bs) = (pairAtDepth (n -1) =<< goLeft t) <|> (pairAtDepth (n -1) =<< goRight t)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node l r, bs) = Just (l, LeftCrumb r : bs)
goLeft _ = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node l r, bs) = Just (r, RightCrumb l : bs)
goRight _ = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb r : bs) = Just (Node t r, bs)
goUp (t, RightCrumb l : bs) = Just (Node l t, bs)
goUp _ = Nothing

goTop :: Zipper a -> Zipper a
goTop t@(_, []) = t
goTop t@(_, _ : _) = maybe t goTop (goUp t)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

-- https://github.com/Vipul97/slr-parser

-- AUGMENTED GRAMMAR:
-- 0: S' -> S
-- 1:  S -> P
-- 2:  P -> [ E , E ]
-- 3:  E -> N
-- 4:  E -> P
-- 5:  N -> r

--    TERMINALS: ,, [, r, ]
-- NONTERMINALS: N, P, S', S, E
--      SYMBOLS: N, ,, [, r, P, S', S, E, ]

-- FIRST:
-- S' = { [ }
--  S = { [ }
--  P = { [ }
--  E = { [, r }
--  N = { r }

-- FOLLOW:
-- S' = { $ }
--  S = { $ }
--  P = { ,, $, ] }
--  E = { ,, ] }
--  N = { ,, ] }

-- PARSING TABLE:
-- +--------+--------------------------------------------+-----------------------------------+

-- |        |                   ACTION                   |               GOTO                |
-- | STATE  +--------+--------+--------+--------+--------+--------+--------+--------+--------+
-- |        |    ,   |    [   |    r   |    ]   |    $   |    N   |    S   |    E   |    P   |
-- +--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+
-- |   0    |        |   s1   |        |        |        |        |    3   |        |    2   |
-- |   1    |        |   s1   |   s5   |        |        |    4   |        |    7   |    6   |
-- |   2    |        |        |        |        |   r1   |        |        |        |        |
-- |   3    |        |        |        |        |   acc  |        |        |        |        |
-- |   4    |   r3   |        |        |   r3   |        |        |        |        |        |
-- |   5    |   r5   |        |        |   r5   |        |        |        |        |        |
-- |   6    |   r4   |        |        |   r4   |        |        |        |        |        |
-- |   7    |   s8   |        |        |        |        |        |        |        |        |
-- |   8    |        |   s1   |   s5   |        |        |    4   |        |    9   |    6   |
-- |   9    |        |        |        |   s10  |        |        |        |        |        |
-- |   10   |   r2   |        |        |   r2   |   r2   |        |        |        |        |
-- +--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+

-- +------+--------------------+------------------+---------------------+--------------------------+

-- |      |       STACK        |     SYMBOLS      |        INPUT        |          ACTION          |
-- +------+--------------------+------------------+---------------------+--------------------------+
-- |  (1) | 0                  |                  | [ r , [ r , r ] ] $ | shift                    |
-- |  (2) | 0 1                |  [               |   r , [ r , r ] ] $ | shift                    |
-- |  (3) | 0 1 5              |  [ r             |     , [ r , r ] ] $ | reduce by N -> r         |
-- |  (4) | 0 1 4              |  [ N             |     , [ r , r ] ] $ | reduce by E -> N         |
-- |  (5) | 0 1 7              |  [ E             |     , [ r , r ] ] $ | shift                    |
-- |  (6) | 0 1 7 8            |  [ E ,           |       [ r , r ] ] $ | shift                    |
-- |  (7) | 0 1 7 8 1          |  [ E , [         |         r , r ] ] $ | shift                    |
-- |  (8) | 0 1 7 8 1 5        |  [ E , [ r       |           , r ] ] $ | reduce by N -> r         |
-- |  (9) | 0 1 7 8 1 4        |  [ E , [ N       |           , r ] ] $ | reduce by E -> N         |
-- | (10) | 0 1 7 8 1 7        |  [ E , [ E       |           , r ] ] $ | shift                    |
-- | (11) | 0 1 7 8 1 7 8      |  [ E , [ E ,     |             r ] ] $ | shift                    |
-- | (12) | 0 1 7 8 1 7 8 5    |  [ E , [ E , r   |               ] ] $ | reduce by N -> r         |
-- | (13) | 0 1 7 8 1 7 8 4    |  [ E , [ E , N   |               ] ] $ | reduce by E -> N         |
-- | (14) | 0 1 7 8 1 7 8 9    |  [ E , [ E , E   |               ] ] $ | shift                    |
-- | (15) | 0 1 7 8 1 7 8 9 10 |  [ E , [ E , E ] |                 ] $ | reduce by P -> [ E , E ] |
-- | (16) | 0 1 7 8 6          |  [ E , P         |                 ] $ | reduce by E -> P         |
-- | (17) | 0 1 7 8 9          |  [ E , E         |                 ] $ | shift                    |
-- | (18) | 0 1 7 8 9 10       |  [ E , E ]       |                   $ | reduce by P -> [ E , E ] |
-- | (19) | 0 2                |  P               |                   $ | reduce by S -> P         |
-- | (20) | 0 3                |  S               |                   $ | accept                   |
-- +------+--------------------+------------------+---------------------+--------------------------+
slrParse :: [Char] -> Tree Int
slrParse input = parse [0] [] input []
  where
    parse states symbols input nodes = parse' (head states) symbols nodes
      where
        c = head input

        parse' 0 symbols nodes
          | c == '[' = parse (push 1 states) (push '[' symbols) (tail input) nodes
          | otherwise = error "Invalid character at state 0!"
        parse' 1 symbols nodes
          | isNumber c = parse (push 5 states) (push c symbols) (tail input) (push (Leaf (read [c] :: Int)) nodes)
          | c == '[' = parse (push 1 states) (push c symbols) (tail input) nodes
          | otherwise = error "Invalid character at state 1!"
        parse' 2 symbols nodes
          | null input = case pop states of
            Just (nextStates, _) -> case pop symbols of
              Just (nextSymbols, _) -> parse (push 3 nextStates) (push 'S' nextSymbols) input nodes
              _ -> error "Invalid state!"
            _ -> error "Invalid state!"
          | otherwise = error "Invalid state!"
        parse' 3 symbols nodes
          | null input = head nodes
          | otherwise = error "Invalid character at state 3!"
        parse' 5 symbols nodes
          | c == ',' = case pop states of
            Just (nextState, state) -> case pop symbols of
              Just (nextSymbols, _) -> parse (push 4 nextState) (push 'N' nextSymbols) input nodes
              _ -> error "Invalid state!"
            _ -> error "Invalid state!"
          | c == ']' = case pop states of
            Just (nextState, state) -> case pop symbols of
              Just (nextSymbols, _) -> parse (push 4 nextState) (push 'N' nextSymbols) input nodes
              _ -> error "Invalid state!"
            _ -> error "Invalid state!"
          | otherwise = error "Invalid character at state 5!"
        parse' 4 symbols nodes
          | c == ',' = case pop states of
            Just (nextState, state) -> case pop symbols of
              Just (nextSymbols, _) -> parse (push 6 states) (push 'E' nextSymbols) input nodes
              _ -> error "Invalid state!"
            _ -> error "Invalid state!"
          | c == ']' = case pop states of
            Just (nextState, state) -> case pop symbols of
              Just (nextSymbols, _) -> parse (push 9 states) (push 'E' nextSymbols) input nodes
              _ -> error "Invalid state!"
            _ -> error "Invalid state!"
          | otherwise = error "Invalid character at state 4!"
        parse' 6 symbols nodes
          | c == ',' = parse (push 8 states) (push ',' symbols) (tail input) nodes
          | c == ']' = case pop states of
            Just (nextState, _) -> case pop symbols of
              Just (nextSymbols, _) -> parse (push 9 states) (push 'E' nextSymbols) input nodes
              _ -> error "Invalid state!"
            _ -> error "Invalid state!"
          | otherwise = error "Invalid character at state 6!"
        parse' 7 symbols nodes
          | c == ',' = case pop states of
            Just (nextState, _) -> case pop symbols of
              Just (nextSymbols, _) -> parse (push 4 nextState) (push 'E' nextSymbols) input nodes
              _ -> error "Invalid state!"
            _ -> error "Invalid state!"
          | otherwise = error "Invalid character at state 7!"
        parse' 8 symbols nodes
          | isNumber c = parse (push 5 states) (push c symbols) (tail input) (push (Leaf (read [c] :: Int)) nodes)
          | c == '[' = parse (push 1 states) (push '[' symbols) (tail input) nodes
          | otherwise = error "Invalid character at state 8!"
        parse' 9 symbols nodes
          | c == ']' = parse (push 10 states) (push ']' symbols) (tail input) nodes
          | otherwise = error "Invalid character at state 9!"
        parse' 10 symbols nodes
          | null input = parse (push 2 nextStates) (push 'P' nextSymbols) input (push node nextNodes)
          | c == ']' = parse (push 6 nextStates) (push 'P' nextSymbols) input (push node nextNodes)
          | c == ',' = parse (push 7 nextStates) (push 'P' nextSymbols) input (push node nextNodes)
          | otherwise = error "Invalid character at state 10!"
          where
            node = nodeFrom (take 2 nodes)
            nextNodes = drop 2 nodes
            nextStates = drop 5 states
            nextSymbols = drop 5 symbols
        parse' _ _ _ = error "Invalid state!"

nodeFrom :: [Tree Int] -> Tree Int
nodeFrom raw = Node left right
  where
    left = last raw
    right = head raw

push :: a -> [a] -> [a]
push c s = c : s

pop :: [a] -> Maybe ([a], a)
pop s
  | null s = Nothing
  | otherwise = Just (tail s, head s)
