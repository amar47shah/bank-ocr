{-# OPTIONS_GHC -Wall #-}
module Digit (Digit, toChar, fromTuple) where

import Control.Monad ((>=>))

-- Exported definitions:

type Digit = Either (ReadError OCR) Char

toChar :: Digit -> Char
toChar = either (const '?') id

fromTuple :: (String, String, String) -> Digit
fromTuple = tryOCR >=> lookupOCR

-- Private definitions:

data ReadError a = Unrecognized a | WrongFormat deriving Show

data OCR = OCR [Bool] deriving Eq

bits :: OCR -> [Bool]
bits (OCR bs) = bs

instance Show OCR where
  show o = '\n' : unlines [[' ', t, ' '], [tl, m, tr], [bl, b, br]]
    where [t, tl, m, tr, bl, b, br] = zipWith decode "_|_||_|" $ bits o
          decode :: Char -> Bool -> Char
          decode c True  = c
          decode _ False = ' '

tryOCR :: (String, String, String) -> Either (ReadError a) OCR
tryOCR ([_ , t, _ ]
       ,[tl, m, tr]
       ,[bl, b, br]) = Right . OCR $ (/= ' ') <$> [t, tl, m, tr, bl, b, br]
tryOCR _             = Left WrongFormat

lookupOCR :: OCR -> Digit
lookupOCR = (`lookup'` table)

lookup' :: Eq a => a -> [(a, b)] -> Either (ReadError a) b
lookup' x ts = case lookup x ts of
                 Just y -> Right y
                 _      -> Left $ Unrecognized x

table :: [(OCR, Char)]
table = [(OCR [ True,  True, False,  True,  True,  True,  True], '0')
        ,(OCR [False, False, False,  True, False, False,  True], '1')
        ,(OCR [ True, False,  True,  True,  True,  True, False], '2')
        ,(OCR [ True, False,  True,  True, False,  True,  True], '3')
        ,(OCR [False,  True,  True,  True, False, False,  True], '4')
        ,(OCR [ True,  True,  True, False, False,  True,  True], '5')
        ,(OCR [ True,  True,  True, False,  True,  True,  True], '6')
        ,(OCR [ True, False, False,  True, False, False,  True], '7')
        ,(OCR [ True,  True,  True,  True,  True,  True,  True], '8')
        ,(OCR [ True,  True,  True,  True, False,  True,  True], '9')
        ]
