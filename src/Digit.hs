{-# OPTIONS_GHC -Wall #-}
module Digit (Digit, alternatives, fromTuple, toChar) where

import Control.Monad ((>=>))
import Data.Either (isRight)
import Data.Tuple (swap)

type Digit = Either ReadError Char

data ReadError = WrongFormat | Unrecognized OCR deriving Show

data OCR = OCR [Bool] deriving Eq

instance Show OCR where
  show (OCR bits) = '\n' : unlines [[' ', t, ' '], [tl, m, tr], [bl, b, br]]
    where [t, tl, m, tr, bl, b, br] = zipWith decode "_|_||_|" bits
          decode c True  = c
          decode _ False = ' '

toChar :: Digit -> Char
toChar = either (const '?') id

fromTuple :: (String, String, String) -> Digit
fromTuple = tryOCR >=> lookupOCR

tryOCR :: (String, String, String) -> Either ReadError OCR
tryOCR ([_ , t, _ ]
       ,[tl, m, tr]
       ,[bl, b, br]) = Right . OCR $ (/= ' ') <$> [t, tl, m, tr, bl, b, br]
tryOCR _             = Left WrongFormat

lookupOCR :: OCR -> Digit
lookupOCR ocr = case lookup ocr table of
                  Just c -> Right c
                  _      -> Left $ Unrecognized ocr

lookupChar :: Char -> Maybe OCR
lookupChar = (`lookup` map swap table)

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

alternatives :: Digit -> [Digit]
alternatives (Left (Unrecognized o)) = variants $ Just o
alternatives (Right c)               = variants $ lookupChar c
alternatives _                       = []

variants :: Maybe OCR -> [Digit]
variants (Just o) = filter isRight . (lookupOCR <$>) . oneAways $ o
variants _        = []

oneAways :: OCR -> [OCR]
oneAways (OCR bits) = let oneAway (a, b:c) = OCR $ a ++ (not b) : c
                          oneAway (a, _)   = OCR a
                       in oneAway <$> zipWith splitAt [0..6] (replicate 7 bits)
