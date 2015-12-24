{-# OPTIONS_GHC -Wall #-}
module Main where

type Text = String
type Chunk = [Text]
type OCR = (Text, Text, Text)
type Digit = Char
type Number = [Digit]

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = first : splitEvery n rest
  where (first, rest) = splitAt n xs

chunks :: Text -> [Chunk]
chunks = splitEvery 4 . lines

parseDigits :: Chunk -> [Maybe Digit]
parseDigits ls@(_:_:_:_:_) = map fromOCR $ zip3 x y z
  where [x, y, z] = map (splitEvery 3) . take 3 $ ls
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

parseNumber :: Chunk -> Maybe Number
parseNumber = sequence . parseDigits

parseAll :: Text -> [Maybe Number]
parseAll = map parseNumber . chunks

pack :: [Maybe Number] -> String
pack = unlines . map toString
  where toString Nothing  = ""
        toString (Just n) = n

main :: IO ()
main =
  readFile "input.txt" >>=
    return . pack . parseAll >>=
      writeFile "output.txt" >>
        putStrLn "done"
