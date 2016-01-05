{-# OPTIONS_GHC -Wall #-}
module Parser (assemble, parse) where

import Digit (Digit, toChar, fromTuple)
import Split (splitEvery)

import Data.Char (digitToInt)
import Data.Either (lefts, rights)

assemble :: [Number] -> String
assemble = unlines . map show

parse :: String -> [Number]
parse = map parseNumber . chunks

type Chunk = [String]
data Number = Number { status :: Status
                     , digits :: [Digit] }

instance Show Number where
  show n = (toChar <$> digits n) ++ tag n
    where tag n'
           | not $ legible n' = " ILL"
           | not $ check n'   = " ERR"
           | otherwise        = ""

data Status = Empty
            | Unverified
            | Verified
            | Illegible
            | Replaced [Digit]
            | Ambiguous [[Digit]]
            deriving Show

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
parseNumber (a:b:c:_:[]) = let [xs, ys, zs] = splitEvery 3 <$> [a, b, c]
                            in Number Unverified $ fromTuple <$> zip3 xs ys zs
parseNumber _            = Number Empty []

chunks :: String -> [Chunk]
chunks = splitEvery 4 . lines
