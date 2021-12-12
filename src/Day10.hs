module Day10 where

import Data.List ( sort )

score :: [[Char]] -> Integer
score =
  sum
    . map points
    . filter (/= "")
    . map firstIncorrect

scorePartTwo :: [[Char]] -> Integer
scorePartTwo raw = scores !! (length scores `div` 2)
  where
    scores =
      sort
        . map (calculateScorePartTwo . completeLine)
        . filter isValidLine
        $ raw

calculateScorePartTwo :: [Char] -> Integer
calculateScorePartTwo = foldl (\curr y -> curr * 5 + pointsPartTwo y) 0

firstIncorrect :: [Char] -> [Char]
firstIncorrect = take 1 . validateLine

isValidLine :: [Char] -> Bool
isValidLine raw = validateLine raw == ""

validateLine :: [Char] -> [Char]
validateLine s = validate' s []
  where
    validate' (x : xs) stack
      | x `elem` openChunks = validate' xs (stack ++ [x])
      | x `elem` closingChunks && x == openToClose (last stack) = validate' xs (init stack)
      | otherwise = x : validate' xs stack
    validate' _ _ = []

completeLine :: [Char] -> [Char]
completeLine s = complete' s []
  where
    complete' [] stack = map openToClose . reverse $ stack
    complete' (x : xs) stack
      | x `elem` openChunks = complete' xs (stack ++ [x])
      | x `elem` closingChunks && x == openToClose (last stack) = complete' xs (init stack)
      | otherwise = x : complete' xs stack

points :: Num p => [Char] -> p
points ")" = 3
points "]" = 57
points "}" = 1197
points ">" = 25137
points _ = 0

pointsPartTwo :: Num p => Char -> p
pointsPartTwo ')' = 1
pointsPartTwo ']' = 2
pointsPartTwo '}' = 3
pointsPartTwo '>' = 4
pointsPartTwo _ = 0

openChunks :: [Char]
openChunks = "([{<"

closingChunks :: [Char]
closingChunks = ")]}>"

openToClose :: Char -> Char
openToClose '(' = ')'
openToClose '[' = ']'
openToClose '{' = '}'
openToClose '<' = '>'
openToClose _ = error "Character is not supported"
