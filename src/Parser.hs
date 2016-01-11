{-# OPTIONS_GHC -Wall #-}
module Parser (assemble, parse) where

import Digit (Digit, alternatives, toChar, fromTuple)
import Split (splitEvery, splits)

import Control.Arrow ((&&&))
import Data.Char (digitToInt)
import Data.Either (lefts, rights)

assemble :: [Number] -> String
assemble = unlines . map show

parse :: String -> [Number]
parse = map process . chunks

type Chunk = [String]

chunks :: String -> [Chunk]
chunks = splitEvery 4 . lines

data Status = Unparsable
            | Unverified
            | Illegible
            | Incorrect
            | Correct
            | Replaced Number
            | Ambiguous [Number]
            deriving Show

tag :: Status -> String
tag Illegible      = " ILL"
tag Incorrect      = " ERR"
tag (Ambiguous ns) = " AMB " ++ show ns
tag _              = ""

data Number = Number { status :: Status
                     , digits :: [Digit] }

instance Show Number where
  show (Number (Replaced n) _) = show n
  show n                       = (display $ digits n) ++ (tag $ status n)

display :: [Digit] -> String
display = map toChar

process :: Chunk -> Number
process = repair . verify . parseNumber

parseNumber :: Chunk -> Number
parseNumber (a:b:c:_:[]) = let [xs, ys, zs] = splitEvery 3 <$> [a, b, c]
                            in new $ fromTuple <$> zip3 xs ys zs
parseNumber _            = Number Unparsable []

new :: [Digit] -> Number
new = Number Unverified

verify :: Number -> Number
verify n@(Number Unverified _)
 | not $ legible n = n { status = Illegible }
 | not $   check n = n { status = Incorrect }
 | otherwise       = n { status = Correct   }
verify n = n

legible :: Number -> Bool
legible = null . lefts . digits

check :: Number -> Bool
check = (== 0)
      . (`mod` 11)
      . toInteger
      . sum
      . zipWith (*) [9,8..1]
      . (digitToInt <$>)
      . rights
      . digits

repair :: Number -> Number
repair n@(Number Illegible _) = repair' n
repair n@(Number Incorrect _) = repair' n
repair n                      = n

repair' :: Number -> Number
repair' = uncurry resolve . (replacements &&& id)

resolve :: [Number] -> Number -> Number
resolve []  n = n
resolve [r] n = n { status = Replaced r }
resolve rs  n = n { status = Ambiguous rs }

replacements :: Number -> [Number]
replacements = filter correct . (verify . new <$>) . alts . digits

correct :: Number -> Bool
correct (Number Correct _) = True
correct _                  = False

alts :: [Digit] -> [[Digit]]
alts = foldr ((++) . oneAways) [] . splits
  where oneAways :: ([Digit], [Digit]) -> [[Digit]]
        oneAways (a, b:c) = [a ++ b':c | b' <- alternatives b]
        oneAways _        = []
