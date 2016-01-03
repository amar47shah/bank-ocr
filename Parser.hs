{-# OPTIONS_GHC -Wall #-}
module Parser (assemble, parse) where

type Chunk = [String]
type OCR = (String, String, String)
type Digit = Either OCR Char

data Number = Number Bool [Digit]

assemble :: [Number] -> String
assemble = unlines . map show

parse :: String -> [Number]
parse = map parseNumber . chunks

instance Show Number where
  show (Number l ds) = tag l $ map toChar ds
    where tag True  = id
          tag False = (++ " ILL")

toChar :: Digit -> Char
toChar = either (const '?') id

parseNumber :: Chunk -> Number
parseNumber (t:m:b:_:[]) = let [x, y, z] = splitEvery 3 <$> [t, m, b]
                            in foldr (|-) empty $ fromOCR <$> zip3 x y z
parseNumber _            = empty

empty :: Number
empty = Number True []

(|-) :: Digit -> Number -> Number
d@(Left _) |- Number _ ds = Number False       $ d:ds
d          |- Number l ds = Number (True && l) $ d:ds

fromOCR :: OCR -> Digit
fromOCR (" _ ","| |","|_|") = Right '0'
fromOCR ("   ","  |","  |") = Right '1'
fromOCR (" _ "," _|","|_ ") = Right '2'
fromOCR (" _ "," _|"," _|") = Right '3'
fromOCR ("   ","|_|","  |") = Right '4'
fromOCR (" _ ","|_ "," _|") = Right '5'
fromOCR (" _ ","|_ ","|_|") = Right '6'
fromOCR (" _ ","  |","  |") = Right '7'
fromOCR (" _ ","|_|","|_|") = Right '8'
fromOCR (" _ ","|_|"," _|") = Right '9'
fromOCR ocr                 = Left ocr

chunks :: String -> [Chunk]
chunks = splitEvery 4 . lines

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = first : splitEvery n rest
  where (first, rest) = splitAt n xs
