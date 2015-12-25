{-# OPTIONS_GHC -Wall #-}
module Parser (assemble, parse) where

import Data.Maybe (fromMaybe)

type Chunk = [String]
type OCR = (String, String, String)
type Digit = Char
type Number = [Digit]

assemble :: [Maybe Number] -> String
assemble = unlines . map (fromMaybe "")

parse :: String -> [Maybe Number]
parse = map parseNumber . chunks

parseNumber :: Chunk -> Maybe Number
parseNumber = sequence . parseDigits

parseDigits :: Chunk -> [Maybe Digit]
parseDigits c@(_:_:_:_:_) = map fromOCR $ zip3 x y z
  where [x, y, z] = map (splitEvery 3) . take 3 $ c
parseDigits _ = []

fromOCR :: OCR -> Maybe Digit
fromOCR o =
 case o of
   (" _ ","| |","|_|") -> Just '0'
   ("   ","  |","  |") -> Just '1'
   (" _ "," _|","|_ ") -> Just '2'
   (" _ "," _|"," _|") -> Just '3'
   ("   ","|_|","  |") -> Just '4'
   (" _ ","|_ "," _|") -> Just '5'
   (" _ ","|_ ","|_|") -> Just '6'
   (" _ ","  |","  |") -> Just '7'
   (" _ ","|_|","|_|") -> Just '8'
   (" _ ","|_|"," _|") -> Just '9'
   _ -> Nothing

chunks :: String -> [Chunk]
chunks = splitEvery 4 . lines

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = first : splitEvery n rest
  where (first, rest) = splitAt n xs
