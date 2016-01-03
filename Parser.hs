{-# OPTIONS_GHC -Wall #-}
module Parser (assemble, parse) where

import OCR (Digit, toChar, fromOCR)
import Split (splitEvery)

assemble :: [Number] -> String
assemble = unlines . map show

parse :: String -> [Number]
parse = map parseNumber . chunks

type Chunk = [String]
data Number = Number Bool [Digit]

instance Show Number where
  show (Number l ds) = tag l $ map toChar ds
    where tag True  = id
          tag False = (++ " ILL")

parseNumber :: Chunk -> Number
parseNumber (t:m:b:_:[]) = let [x, y, z] = splitEvery 3 <$> [t, m, b]
                            in foldr (|-) empty $ fromOCR <$> zip3 x y z
parseNumber _            = empty

empty :: Number
empty = Number True []

(|-) :: Digit -> Number -> Number
d@(Left _) |- Number _ ds = Number False       $ d:ds
d          |- Number l ds = Number (True && l) $ d:ds

chunks :: String -> [Chunk]
chunks = splitEvery 4 . lines
