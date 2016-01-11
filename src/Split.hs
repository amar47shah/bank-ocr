{-# OPTIONS_GHC -Wall #-}
module Split (splitEvery, splits) where

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = first : splitEvery n rest
  where (first, rest) = splitAt n xs

splits :: [a] -> [([a], [a])]
splits xs = let n = length xs
             in zipWith splitAt [0..pred n] $ replicate n xs
