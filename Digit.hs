{-# OPTIONS_GHC -Wall #-}
module Digit (Digit, fromTuple, toChar) where

import Data.List (find)

type Digit = Either (Maybe OCR) Char
data OCR = OCR [Bool] deriving Eq

instance Show OCR where
  show (OCR bits) = '\n' : unlines [[' ', t, ' '], [tl, m, tr], [bl, b, br]]
    where [t, tl, m, tr, bl, b, br] = zipWith decode "_|_||_|" bits
          decode c True  = c
          decode _ False = ' '

toChar :: Digit -> Char
toChar = either (const '?') id

fromTuple :: (String, String, String) -> Digit
fromTuple = zipZap tryOCR lookupOCR

lookupOCR :: OCR -> Maybe Char
lookupOCR o = snd <$> find ((== o) . fst) table

-- No idea what to call this.
-- There must be a way to simplify it
-- using Functor-Applicative-Monad
-- instances for Maybe and Either.
zipZap :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Either (Maybe b) c
zipZap f g x = case f x of
                 Nothing -> Left Nothing
                 Just y  -> case g y of
                              Nothing -> Left $ Just y
                              Just z  -> Right z

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

tryOCR :: (String, String, String) -> Maybe OCR
tryOCR ([_ , t, _ ]
       ,[tl, m, tr]
       ,[bl, b, br]) = Just . OCR $ (/= ' ') <$> [t, tl, m, tr, bl, b, br]
tryOCR _             = Nothing

variants :: OCR -> [OCR]
variants (OCR bits) = let vary (a, b:c) = OCR $ a ++ (not b) : c
                          vary (a, _)   = OCR a
                       in vary <$> zipWith splitAt [0..6] (replicate 7 bits)
