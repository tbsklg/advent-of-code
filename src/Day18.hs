{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day18 where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Maybe
import Debug.Trace

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

reduce :: Zipper Int -> Zipper Int
reduce t@(Node _ _, []) = case explode t of
  Just t' -> reduce t'
  Nothing -> maybe (goTop t) reduce (split t)

explode :: Zipper Int -> Maybe (Zipper Int)
explode t@(Node _ _, bs) = case pairAt4 of
  Just a -> Just next
  Nothing -> Nothing
  where
    pairAt4 = pairAtDepth 4 t
    t1@(Node (Leaf ll) (Leaf rl), _) = fromJust pairAt4

    newL = case leftMost t1 of
      Just t2@(Leaf ll', _) -> attach (Leaf (ll' + ll)) t2
      Just (_, _) -> t1
      Nothing -> t1

    newR = case pairAtDepth 4 (goTop newL) >>= rightMost of
      Just t2@(Leaf rl', _) -> attach (Leaf (rl' + rl)) t2
      Just (_, _) -> newL
      Nothing -> newL

    repl = case pairAtDepth 4 (goTop newR) of
      Just t -> attach (Leaf 0) t
      Nothing -> t

    next = goTop repl

leftMost :: Zipper Int -> Maybe (Zipper Int)
leftMost t@(_, []) = Nothing
leftMost t@(_, LeftCrumb _ : bs) = leftMost =<< goUp t
leftMost t@(_, RightCrumb _ : bs) = rightMostForLeft =<< goUp t

rightMostForLeft :: Zipper Int -> Maybe (Zipper Int)
rightMostForLeft t@(Leaf _, _) = Just t
rightMostForLeft t@(Node (Leaf _) _, bs) = rightMostForLeft =<< goLeft t
rightMostForLeft t@(Node _ _, bs) = rightMostForLeft =<< goLeft t

rightMost :: Zipper Int -> Maybe (Zipper Int)
rightMost t@(_, []) = Nothing
rightMost t@(_, RightCrumb _ : bs) = rightMost =<< goUp t
rightMost t@(_, LeftCrumb _ : bs) = leftMostForRight =<< goUp t

leftMostForRight :: Zipper Int -> Maybe (Zipper Int)
leftMostForRight t@(Leaf _, _) = Just t
leftMostForRight t@(Node (Leaf _) _, bs) = leftMostForRight =<< goLeft t
leftMostForRight t@(Node _ _, bs) = leftMostForRight =<< goRight t

split :: Zipper Int -> Maybe (Zipper Int)
split t = case splittable t of
  Just t@(Leaf a, bs) -> Just . goTop . attach nN $ t
    where
      nN = Node (Leaf lP) (Leaf rP)
      lP = a `div` 2
      rP = ceiling (fromIntegral a / 2)
  Just t@(_, _) -> Nothing
  Nothing -> Nothing

splittable :: Zipper Int -> Maybe (Zipper Int)
splittable t@(Leaf a, _)
  | a >= 10 = Just t
  | otherwise = Nothing
splittable t@(Node _ _, _) = (splittable =<< goRight t) <|> (splittable =<< goLeft t)

pairAtDepth :: Int -> Zipper Int -> Maybe (Zipper Int)
pairAtDepth n (Leaf _, _) = Nothing
pairAtDepth 0 (t, bs) = Just (t, bs)
pairAtDepth n (Node l (Leaf a), bs) = case goLeft (Node l (Leaf a), bs) of
  Just next -> pairAtDepth (n - 1) next
  Nothing -> Nothing
pairAtDepth n (Node (Leaf a) r, bs) = case goRight (Node (Leaf a) r, bs) of
  Just next -> pairAtDepth (n - 1) next
  Nothing -> Nothing
pairAtDepth n (Node l r, bs)
  | maxDepth' l >= maxDepth' r = case goLeft (Node l r, bs) of
    Just next -> pairAtDepth (n - 1) next
    Nothing -> Nothing
  | otherwise = case goRight (Node l r, bs) of
    Just next -> pairAtDepth (n - 1) next
    Nothing -> Nothing

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

parse :: [Char] -> Tree Int
parse raw = parse' raw [] [] []
  where
    parse' [] [] [] tree = case pop tree of
      Just (_, result) -> result
      _ -> error "Cannot parse raw data!"
    parse' (x : xs) arr num tree
      | x == '[' = parse' xs (push '[' arr) num tree
      | isDigit x =
        if head xs == ','
          then parse' xs arr (push (read [x] :: Int, L) num) tree
          else parse' xs arr (push (read [x] :: Int, R) num) tree
      | x == ']' = case pop arr of
        Just (newArr, _) -> case pop num of
          Just (newNum, (right, d1)) ->
            if d1 == L
              then case pop tree of
                Just (newTree, t1) -> parse' xs newArr newNum (push (Node (Leaf right) t1) newTree)
                _ -> error "Invalid state!"
              else case pop newNum of
                Just (newNum', (left, d2)) -> parse' xs newArr newNum' (push (Node (Leaf left) (Leaf right)) tree)
                _ -> case pop tree of
                  Just (newTree, t1) ->
                    if d1 == R
                      then parse' xs newArr newNum (push (Node t1 (Leaf right)) newTree)
                      else parse' xs newArr newNum (push (Node (Leaf right) t1) newTree)
                  _ -> error "Invalid state!"
          _ -> case pop tree of
            Just (newTree, t1) -> case pop newTree of
              Just (newTree', t2) -> parse' xs newArr num (push (Node t2 t1) newTree')
              _ -> error "Invalid state!"
            _ -> error "Invalid state!"
        _ -> error "Invalid state!"
      | otherwise = parse' xs arr num tree
    parse' _ _ _ _ = error "Cannot parse raw data!"

push :: a -> [a] -> [a]
push c s = c : s

pop :: [a] -> Maybe ([a], a)
pop s
  | null s = Nothing
  | otherwise = Just (tail s, head s)
