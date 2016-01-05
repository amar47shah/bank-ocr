{-# OPTIONS_GHC -Wall #-}
module Parser (assemble, parse) where

import Digit (Digit, toChar, fromTuple)
import Split (splitEvery)

import Data.Char (digitToInt)
import Data.Either (lefts, rights)

assemble :: [Number] -> String
assemble = unlines . map show

parse :: String -> [Number]
parse = map process . chunks

type Chunk = [String]
data Number = Number { status :: Status
                     , digits :: [Digit] }

instance Show Number where
  show n = (toChar <$> digits n) ++ (tag $ status n)

tag :: Status -> String
tag Illegible = " ILL"
tag Incorrect = " ERR"
tag _         = ""

data Status = Empty
            | Unverified
            | Illegible
            | Incorrect
            | Correct
            | Replaced [Digit]
            | Ambiguous [[Digit]]
            deriving Show

process :: Chunk -> Number
process = verify . parseNumber

verify :: Number -> Number
verify n@(Number Unverified _)
 | not $ legible n = n { status = Illegible }
 | not $ check n   = n { status = Incorrect }
 | otherwise    = n { status = Correct   }
verify n = n

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
