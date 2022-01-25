module Day24 where

import Data.Char (isDigit)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe, step)

type PU = M.Map Char Int

data Token = Inp | Add | Mul | Div | Mod | Eql deriving (Show, Ord, Eq)

digitsAsc :: [[Int]]
digitsAsc =
  [ [a, b, c, d, e, f, g]
    | a <- [1 .. 9],
      b <- [1 .. 9],
      c <- [1 .. 9],
      d <- [1 .. 9],
      e <- [1 .. 9],
      f <- [1 .. 9],
      g <- [1 .. 9]
  ]

digitsDsc :: [[Int]]
digitsDsc =
  [ [a, b, c, d, e, f, g]
    | a <- [9, 8 .. 1],
      b <- [9, 8 .. 1],
      c <- [9, 8 .. 1],
      d <- [9, 8 .. 1],
      e <- [9, 8 .. 1],
      f <- [9, 8 .. 1],
      g <- [9, 8 .. 1]
  ]

solve :: p -> Int
solve _ =
  read
    ( concatMap show
        . head
        . filter (\x -> length x == 14)
        . map (\x -> simulate' x 0 0 0)
        $ digitsDsc
    ) ::
    Int

solvePartTwo :: p -> Int
solvePartTwo _ =
  read
    ( concatMap show
        . head
        . filter (\x -> length x == 14)
        . map (\x -> simulate' x 0 0 0)
        $ digitsAsc
    ) ::
    Int

-- z = z * 26 + w + a
increment :: [Maybe Int]
increment = [Just 6, Just 7, Just 10, Just 2, Nothing, Just 8, Just 1, Nothing, Just 5, Nothing, Nothing, Nothing, Nothing, Nothing]

-- w = z `mod` 26 + a
-- z = z `div` 26 if w is between range 1 and 9
decrement :: [Maybe Int]
decrement = [Nothing, Nothing, Nothing, Nothing, Just (-7), Nothing, Nothing, Just (-5), Nothing, Just (-3), Just 0, Just (-5), Just (-9), Just 0]

simulate' :: [Int] -> Int -> Int -> Int -> [Int]
simulate' digits z 14 _ = []
simulate' digits z step index = case increment !! step of
  Just a -> digit : simulate' digits nextZ (step + 1) (index + 1)
    where
      digit = digits !! index
      nextZ = z * 26 + digit + a
  Nothing -> case decrement !! step of
    Just b ->
      if isInRange
        then digit : simulate' digits nextZ (step + 1) index
        else []
      where
        isInRange = (z `mod` 26 + b) >= 1 && (z `mod` 26 + b) <= 9
        digit = z `mod` 26 + b
        nextZ = z `div` 26
    Nothing -> error "Invalid state!"

-- This solution is based on the input and uses all ALU instructions.
parse :: [[Char]] -> [Int] -> PU
parse raw nums = parse' raw (M.empty, nums)
  where
    parse' [] (pu, _) = pu
    parse' (x : xs) (pu, nums) = parse' xs nextPU
      where
        nextPU = interpret x (pu, nums)

interpret :: [Char] -> (PU, [Int]) -> (PU, [Int])
interpret line (pu, nums)
  | instruction == Inp = (M.insert variable (head nums) pu, tail nums)
  | otherwise = case M.lookup instruction instructions of
    Just fn ->
      let newValue = fn a b
       in (M.insert variable newValue pu, nums)
    _ -> error "No instruction for token!"
  where
    instruction = fromRaw . head . words $ line

    variable = head . head . tail . words $ line
    a = readValue pu variable

    b' = last . words $ line
    b = case readMaybe b' of
      Just a -> a
      _ -> readValue pu (head b')

readValue :: PU -> Char -> Int
readValue pu a = case M.lookup a pu of
  Just a -> a
  _ -> 0

fromRaw :: String -> Token
fromRaw "inp" = Inp
fromRaw "mul" = Mul
fromRaw "add" = Add
fromRaw "mod" = Mod
fromRaw "div" = Div
fromRaw "eql" = Eql
fromRaw _ = error "Cannot create token from raw input!"

instructions :: M.Map Token (Int -> Int -> Int)
instructions =
  M.fromList
    [ (Add, (+)),
      (Mul, (*)),
      (Div, quot),
      (Mod, mod),
      (Eql, \x y -> if x == y then 1 else 0)
    ]

isValid :: PU -> Bool
isValid pu = readValue pu 'z' == 0
