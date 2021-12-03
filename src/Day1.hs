module Day1 where
import Data.Bifunctor ( Bifunctor(second) )

countIncreases :: Num c => Int -> [Char] -> c
countIncreases n l =
  fst
    . foldl (\x y -> if current x y < next x y then (fst x + 1, tail $ snd x) else second tail x) (0, tail lines')
    $ lines'
  where
    current x y = sum . (:) y . take (n - 1) . snd $ x
    next x y = sum . take n . snd $ x
    lines' = map (\x -> read x :: Int) . lines $ l
