{-# OPTIONS_GHC -Wall #-}
module Parser (assemble, parse) where

type Chunk = [String]
type OCR = (String, String, String)

data Digit = Legible Char | Illegible OCR deriving Show
data Number = Number { digits :: [Digit] }

assemble :: [Number] -> String
assemble = unlines . map show

parse :: String -> [Number]
parse = map parseNumber . chunks

instance Show Number where
  show = map toChar . digits

toChar :: Digit -> Char
toChar (Legible d) = d
toChar _           = '?'

parseNumber :: Chunk -> Number
parseNumber (t:m:b:_:[]) = let [x, y, z] = splitEvery 3 <$> [t, m, b]
                            in Number $ fromOCR <$> zip3 x y z
parseNumber _            = Number []

fromOCR :: OCR -> Digit
fromOCR (" _ ","| |","|_|") = Legible '0'
fromOCR ("   ","  |","  |") = Legible '1'
fromOCR (" _ "," _|","|_ ") = Legible '2'
fromOCR (" _ "," _|"," _|") = Legible '3'
fromOCR ("   ","|_|","  |") = Legible '4'
fromOCR (" _ ","|_ "," _|") = Legible '5'
fromOCR (" _ ","|_ ","|_|") = Legible '6'
fromOCR (" _ ","  |","  |") = Legible '7'
fromOCR (" _ ","|_|","|_|") = Legible '8'
fromOCR (" _ ","|_|"," _|") = Legible '9'
fromOCR ocr                 = Illegible ocr

chunks :: String -> [Chunk]
chunks = splitEvery 4 . lines

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = first : splitEvery n rest
  where (first, rest) = splitAt n xs
