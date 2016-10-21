{-# OPTIONS_GHC -Wall #-}
module Parser (assemble, parse) where

import Digit (Digit, toChar, fromTuple)
import Split (splitEvery)

-- Exported definitions:

assemble :: [Number] -> String
assemble = unlines . map show

parse :: String -> [Number]
parse = map parseNumber . chunks

-- Private definitions:

type Chunk = [String]

chunks :: String -> [Chunk]
chunks = splitEvery 4 . lines

data Number = Number { digits :: [Digit] }

instance Show Number where
  show = display . digits

display :: [Digit] -> String
display = map toChar

parseNumber :: Chunk -> Number
parseNumber [a, b, c, _] = let [xs, ys, zs] = splitEvery 3 <$> [a, b, c]
                            in Number $ fromTuple <$> zip3 xs ys zs
parseNumber _            = error "Entry contains fewer than four lines."
