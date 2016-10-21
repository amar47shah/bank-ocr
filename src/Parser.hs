{-# OPTIONS_GHC -Wall #-}
module Parser (assemble, parse) where

import Digit (Digit, toChar, fromTuple, chars, errors)
import Split (splitEvery)

import Data.Char (digitToInt)

-- Exported definitions:

assemble :: [Number] -> String
assemble = unlines . map show

parse :: String -> [Number]
parse = map process . chunks

-- Private definitions:

type Chunk = [String]

chunks :: String -> [Chunk]
chunks = splitEvery 4 . lines

data Status = Unparsable
            | Unverified
            | Illegible
            | Incorrect
            | Correct
            deriving Show

tag :: Status -> String
tag Illegible      = " ILL"
tag Incorrect      = " ERR"
tag _              = ""

data Number = Number { status :: Status
                     , digits :: [Digit] }

instance Show Number where
  show n = display (digits n) ++ tag (status n)

display :: [Digit] -> String
display = map toChar

process :: Chunk -> Number
process = verify . parseNumber

parseNumber :: Chunk -> Number
parseNumber [a, b, c, _] = let [xs, ys, zs] = splitEvery 3 <$> [a, b, c]
                            in new $ fromTuple <$> zip3 xs ys zs
parseNumber _            = Number Unparsable []

new :: [Digit] -> Number
new = Number Unverified

verify :: Number -> Number
verify n@(Number Unverified _)
 | not $ legible n = n { status = Illegible }
 | not $   check n = n { status = Incorrect }
 | otherwise       = n { status = Correct   }
verify n = n

legible :: Number -> Bool
legible = null . errors . digits

check :: Number -> Bool
check = (== 0)
      . (`mod` 11)
      . toInteger
      . sum
      . zipWith (*) [9,8..1]
      . (digitToInt <$>)
      . chars
      . digits
