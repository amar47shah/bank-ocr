{-# OPTIONS_GHC -Wall #-}
module Digit (Digit, fromTuple, toChar) where

type Digit = Either OCR Char

toChar :: Digit -> Char
toChar = either (const '?') id

type OCR = (String, String, String)

fromTuple :: OCR -> Digit
fromTuple (" _ "
          ,"| |"
          ,"|_|") = Right '0'
fromTuple ("   "
          ,"  |"
          ,"  |") = Right '1'
fromTuple (" _ "
          ," _|"
          ,"|_ ") = Right '2'
fromTuple (" _ "
          ," _|"
          ," _|") = Right '3'
fromTuple ("   "
          ,"|_|"
          ,"  |") = Right '4'
fromTuple (" _ "
          ,"|_ "
          ," _|") = Right '5'
fromTuple (" _ "
          ,"|_ "
          ,"|_|") = Right '6'
fromTuple (" _ "
          ,"  |"
          ,"  |") = Right '7'
fromTuple (" _ "
          ,"|_|"
          ,"|_|") = Right '8'
fromTuple (" _ "
          ,"|_|"
          ," _|") = Right '9'
fromTuple ocr     = Left ocr
