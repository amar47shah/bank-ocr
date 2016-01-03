{-# OPTIONS_GHC -Wall #-}
module Parser (assemble, parse) where

import OCR (Digit, toChar, fromOCR)
import Split (splitEvery)

import Data.Char (digitToInt)
import Data.Either (rights)

assemble :: [Number] -> String
assemble = unlines . map show

parse :: String -> [Number]
parse = map parseNumber . chunks

type Chunk = [String]
data Number = Number Bool [Digit]

instance Show Number where
  show (Number l ds) = tag l ds $ map toChar ds
    where tag True ds'
           | check ds' = id
           | otherwise = (++ " ERR")
          tag False _  = (++ " ILL")

check :: [Digit] -> Bool
check = (== 0)
      . (`mod` 11)
      . toInteger
      . sum
      . zipWith (*) [9,8..1]
      . take 9
      . (digitToInt <$>)
      . rights

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
