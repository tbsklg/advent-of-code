module Day14 where

import Data.List.Split (splitOn)
import Data.Map (empty, fromList, fromListWith, insertWith, toList, unionWith)
import Data.Maybe ( fromMaybe )

solve :: Int -> [[Char]]  -> Int
solve times raw = mostCommon - leastCommon
  where
    mostCommon = maximum . map snd $ polymer'
    leastCommon = minimum . map snd $ polymer'

    template = fst . transform . extract $ raw
    rules = snd . transform . extract $ raw

    polymer' = polymer rules template times

extract :: [[Char]] -> ([Char], [[Char]])
extract raw = (template, rules)
  where
    template = head . takeWhile (/= "") $ raw
    rules = tail . dropWhile (/= "") $ raw

transform :: (a, [[Char]]) -> (a, [([Char], [Char])])
transform raw = (template, toList rules)
  where
    template = fst raw
    rules = fromList . map (\x -> (from x, to x)) . snd $ raw
    from x = head . splitOn " -> " $ x
    to x = last . splitOn " -> " $ x

polymer :: [([Char], [Char])] -> [Char] -> Int -> [(Char, Int)]
polymer rules template times = polymer' (getRulesFromTemplate template) (frequencies template) times
  where
    polymer' _ charCount 0 = charCount
    polymer' rulesCount charCount times = polymer' nextRulesCount nextCharCount (times - 1)
      where
        currentCharCount = countCharsFromRules rules rulesCount
        nextCharCount = toList . unionWith (+) (fromList currentCharCount) $ fromList charCount

        nextRulesCount = extractRules . executeRules rules $ rulesCount

countCharsFromRules :: [([Char], [Char])] -> [([Char], Int)] -> [(Char, Int)]
countCharsFromRules rules input = frequencies . concatMap (\x -> [last . take 2 $ x]) $ executedRules
  where
    executedRules = executeRules rules input

extractRules :: [[Char]] -> [([Char], Int)]
extractRules = frequencies . concatMap (\x -> [first x, second x])
  where
    first x = head x : [head . tail $ x]
    second x = (head . tail $ x) : [last x]

getRulesFromTemplate :: [Char] -> [([Char], Int)]
getRulesFromTemplate input = countRules' input empty
  where
    countRules' [x, y] counts = toList (insertWith (+) (x : [y]) 1 counts)
    countRules' (x : y : ys) counts = countRules' (y : ys) (insertWith (+) (x : [y]) 1 counts)
    countRules' _ _ = []

executeRules :: [([Char], [Char])] -> [([Char], Int)] -> [[Char]]
executeRules rules = concatMap (executeRule rules)

executeRule :: [([Char], [Char])] -> ([Char], Int) -> [[Char]]
executeRule rules execution = execute' template times
  where
    execute' _ 0 = []
    execute' template times = transformation : execute' template (times -1)

    template = fst execution
    times = snd execution
    ruleOutput = fromMaybe "" (lookup template rules)
    transformation = [head template] ++ ruleOutput ++ [last template]

frequencies :: (Ord k, Num a) => [k] -> [(k, a)]
frequencies i = toList $ fromListWith (+) [(c, 1) | c <- i]
