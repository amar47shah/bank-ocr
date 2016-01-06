{-# OPTIONS_GHC -Wall #-}
module Main where

import Parser (assemble, parse)
import qualified Data.ByteString.Lazy.Char8
       as BS (pack, readFile, unpack, writeFile)

main :: IO ()
main =
  BS.readFile "input.txt" >>=
    return . BS.pack . assemble . parse . BS.unpack >>=
      BS.writeFile "output.txt" >>
        putStrLn "Done"
