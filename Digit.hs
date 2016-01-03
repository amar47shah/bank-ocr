{-# OPTIONS_GHC -Wall #-}
module Digit (Digit, fromTuple, toChar) where

type Digit = Either (Maybe OCR) Char
data OCR = OCR [Bool]

instance Show OCR where
  show (OCR bits) = '\n' : unlines [[' ', t, ' '], [tl, m, tr], [bl, b, br]]
    where [t, tl, m, tr, bl, b, br] = zipWith decode "_|_||_|" bits
          decode c True  = c
          decode _ False = ' '

toChar :: Digit -> Char
toChar = either (const '?') id

fromTuple :: (String, String, String) -> Digit
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
fromTuple t       = Left $ tryOCR t

tryOCR :: (String, String, String) -> Maybe OCR
tryOCR ([_ , t, _ ]
       ,[tl, m, tr]
       ,[bl, b, br]) = Just . OCR $ (/= ' ') <$> [t, tl, m, tr, bl, b, br]
tryOCR _             = Nothing

variants :: OCR -> [OCR]
variants (OCR bits) = let vary (a, b:c) = OCR $ a ++ (not b) : c
                          vary (a, _)   = OCR a
                       in vary <$> zipWith splitAt [0..6] (replicate 7 bits)
