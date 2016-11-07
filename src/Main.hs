module Main where

import Parser (assemble, parse)
import qualified Data.ByteString.Lazy.Char8
       as BS (ByteString, pack, readFile, unpack, writeFile)

main :: IO ()
main =
  translate <$>
    BS.readFile "data/input.txt" >>=
      BS.writeFile "data/output.txt" >>
        putStrLn "Done"

translate :: BS.ByteString -> BS.ByteString
translate = BS.pack . assemble . parse . BS.unpack
