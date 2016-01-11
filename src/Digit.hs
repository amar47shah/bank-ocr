{-# OPTIONS_GHC -Wall #-}
module Digit (Digit, toChar, fromTuple, chars, errors, alternatives) where

import Split (splits)

import Control.Monad ((>=>))
import Data.Either (isRight, lefts, rights)
import Data.Tuple (swap)

type Digit = Either (ReadError OCR) Char

chars :: [Digit] -> [Char]
chars = rights

errors :: [Digit] -> [ReadError OCR]
errors = lefts

data ReadError a = Unrecognized a | WrongFormat deriving Show

data OCR = OCR [Bool] deriving Eq

instance Show OCR where
  show (OCR bits) = '\n' : unlines [[' ', t, ' '], [tl, m, tr], [bl, b, br]]
    where [t, tl, m, tr, bl, b, br] = zipWith decode "_|_||_|" bits
          decode :: Char -> Bool -> Char
          decode c True  = c
          decode _ False = ' '

toChar :: Digit -> Char
toChar = either (const '?') id

fromTuple :: (String, String, String) -> Digit
fromTuple = tryOCR >=> lookupOCR

tryOCR :: (String, String, String) -> Either (ReadError a) OCR
tryOCR ([_ , t, _ ]
       ,[tl, m, tr]
       ,[bl, b, br]) = Right . OCR $ (/= ' ') <$> [t, tl, m, tr, bl, b, br]
tryOCR _             = Left WrongFormat

lookupOCR :: OCR -> Digit
lookupOCR = (`lookup'` table)

lookupChar :: Char -> Either (ReadError Char) OCR
lookupChar = (`lookup'` map swap table)

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

alternatives :: Digit -> [Digit]
alternatives (Right c)               = variants $ lookupChar c
alternatives (Left (Unrecognized o)) = variants $ Right o
alternatives _                       = []

variants :: Either a OCR -> [Digit]
variants = either (const []) $ filter isRight . (lookupOCR <$>) . oneAways

oneAways :: OCR -> [OCR]
oneAways (OCR bits) = oneAway <$> splits bits
  where oneAway :: ([Bool], [Bool]) -> OCR
        oneAway (a, b:c) = OCR $ a ++ (not b) : c
        oneAway (a, _)   = OCR a
