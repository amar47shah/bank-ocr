{-# OPTIONS_GHC -Wall #-}
module Parser (assemble, parse) where

import OCR (Digit, toChar, fromOCR)
import Split (splitEvery)

import Data.Char (digitToInt)
import Data.Either (lefts, rights)

assemble :: [Number] -> String
assemble = unlines . map show

parse :: String -> [Number]
parse = map parseNumber . chunks

type Chunk = [String]
data Number = Number { digits :: [Digit] }

instance Show Number where
  show n = (toChar <$> digits n) ++ tag n
    where tag n'
           | not $ legible n' = " ILL"
           | not $ check n'   = " ERR"
           | otherwise        = ""

legible :: Number -> Bool
legible = null . lefts . digits

check :: Number -> Bool
check = (== 0)
      . (`mod` 11)
      . toInteger
      . sum
      . zipWith (*) [9,8..1]
      . (digitToInt <$>)
      . rights
      . digits

parseNumber :: Chunk -> Number
parseNumber (t:m:b:_:[]) = let [x, y, z] = splitEvery 3 <$> [t, m, b]
                            in Number $ fromOCR <$> zip3 x y z
parseNumber _            = Number []

chunks :: String -> [Chunk]
chunks = splitEvery 4 . lines
