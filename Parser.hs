{-# OPTIONS_GHC -Wall #-}
module Parser (assemble, parse) where

type Chunk = [String]
type OCR = (String, String, String)
type Digit = Char
type Number = [Digit]

assemble :: [Number] -> String
assemble = unlines

parse :: String -> [Number]
parse = map parseDigits . chunks

parseDigits :: Chunk -> Number
parseDigits c@(_:_:_:_:_) = map fromOCR $ zip3 x y z
  where [x, y, z] = map (splitEvery 3) . take 3 $ c
parseDigits _ = []

fromOCR :: OCR -> Digit
fromOCR o =
  case o of
    (" _ ","| |","|_|") -> '0'
    ("   ","  |","  |") -> '1'
    (" _ "," _|","|_ ") -> '2'
    (" _ "," _|"," _|") -> '3'
    ("   ","|_|","  |") -> '4'
    (" _ ","|_ "," _|") -> '5'
    (" _ ","|_ ","|_|") -> '6'
    (" _ ","  |","  |") -> '7'
    (" _ ","|_|","|_|") -> '8'
    (" _ ","|_|"," _|") -> '9'
    _                   -> '?'

chunks :: String -> [Chunk]
chunks = splitEvery 4 . lines

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = first : splitEvery n rest
  where (first, rest) = splitAt n xs
