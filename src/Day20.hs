module Day20 where

extract :: [[Char]] -> (String, [String])
extract raw = (iea, img)
    where
        iea = head raw
        img = tail . tail $ raw
