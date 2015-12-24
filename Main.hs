{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Lazy.Char8 as BS

type OCR = (String, String, String)
type Digit = Char
type Number = [Digit]

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = first : splitEvery n rest
  where (first, rest) = splitAt n xs

chunks :: ByteString -> [[String]]
chunks = splitEvery 4 . map BS.unpack . BS.split '\n'

parseDigits :: [String] -> [Maybe Digit]
parseDigits c@(_:_:_:_:_) = map fromOCR $ zip3 x y z
  where [x, y, z] = map (splitEvery 3) . take 3 $ c
parseDigits _ = []

fromOCR :: OCR -> Maybe Digit
fromOCR o =
 case o of
   (" _ ","| |","|_|") -> Just '0'
   ("   ","  |","  |") -> Just '1'
   (" _ "," _|","|_ ") -> Just '2'
   (" _ "," _|"," _|") -> Just '3'
   ("   ","|_|","  |") -> Just '4'
   (" _ ","|_ "," _|") -> Just '5'
   (" _ ","|_ ","|_|") -> Just '6'
   (" _ ","  |","  |") -> Just '7'
   (" _ ","|_|","|_|") -> Just '8'
   (" _ ","|_|"," _|") -> Just '9'
   _ -> Nothing

parseNumber :: [String] -> Maybe Number
parseNumber = sequence . parseDigits

parseAll :: ByteString -> [Maybe Number]
parseAll = map parseNumber . chunks

pack :: [Maybe Number] -> String
pack = unlines . map (fromMaybe "")

main :: IO ()
main =
  BS.readFile "input.txt" >>=
    return . BS.pack . pack . parseAll >>=
      BS.writeFile "output.txt" >>
        putStrLn "done"
