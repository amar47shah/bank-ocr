{-# OPTIONS_GHC -Wall #-}
module OCR (Digit, fromOCR, toChar) where

type OCR = (String, String, String)
type Digit = Either OCR Char

toChar :: Digit -> Char
toChar = either (const '?') id

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
