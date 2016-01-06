{-# OPTIONS_GHC -Wall #-}
module Split (splitEvery) where

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = first : splitEvery n rest
  where (first, rest) = splitAt n xs
