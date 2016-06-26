{-
Copyright (c) 2016, Mickey Vänskä <mvaenskae@gmail.com>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL MICKEY VÄNSKÄ BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

import Control.Monad
import Data.List hiding (delete)
import Numeric
import System.Environment
import System.IO

import Data.Map.Strict (assocs, delete, elems, fromList, mapKeys, toList)

-- Tag delimiter for values
realStart :: [Char]
realStart = "<real>"

-- Tag delimiter for values
realEnd :: [Char]
realEnd = "</real>"

-- Tag delimiter for keys
keyStart :: [Char]
keyStart = "<key>"

-- Tag delimiter for keys
keyEnd :: [Char]
keyEnd = "</key>"

-- Field value for single colour channels
component :: [Char]
component = "Component"

-- Field value indicating whether the colour space is calibrated or not
colspace :: [Char]
colspace = "Color Space"

-- Normalization const to convert real to int
denormalizer :: Double
denormalizer = 255

-- Hexadecimal converter
hexChar number
  | number < 10  = show number
  | number == 10 = "A"
  | number == 11 = "B"
  | number == 12 = "C"
  | number == 13 = "D"
  | number == 14 = "E"
  | number == 15 = "F"
  | otherwise    = error "Hex too large"

-- Map keys to numbers such that we can order them
keyConvert "Ansi 0 Color" = 0
keyConvert "Ansi 1 Color" = 1
keyConvert "Ansi 2 Color" = 2
keyConvert "Ansi 3 Color" = 3
keyConvert "Ansi 4 Color" = 4
keyConvert "Ansi 5 Color" = 5
keyConvert "Ansi 6 Color" = 6
keyConvert "Ansi 7 Color" = 7
keyConvert "Ansi 8 Color" = 8
keyConvert "Ansi 9 Color" = 9
keyConvert "Ansi 10 Color" = 10
keyConvert "Ansi 11 Color" = 11
keyConvert "Ansi 12 Color" = 12
keyConvert "Ansi 13 Color" = 13
keyConvert "Ansi 14 Color" = 14
keyConvert "Ansi 15 Color" = 15
keyConvert "Background Color" = 256
keyConvert "Foreground Color" = 257
keyConvert "Cursor Color" = 258
keyConvert "Cursor Text Color" = 259
keyConvert "Selected Text Color" = 260
keyConvert "Selection Color" = 261
keyConvert "Bold Color" = 262
keyConvert "Badge Color" = 1000
keyConvert "Cursor Guide Color" = 1001
keyConvert "Link Color" = 1002
keyConvert "Tab Color" = 1003

-- Return all lines of file named 'path'
getLines :: FilePath -> IO [[Char]]
getLines path = do
  contents <- readFile path
  return (lines contents)

-- Filter lines by 'tag'
getFiltered :: [Char] -> [[Char]] -> [[Char]]
getFiltered tag = filter (isInfixOf tag)

-- Return lines not matching the filter
removeFiltered :: [Char] -> [[Char]] -> [[Char]]
removeFiltered tag = filter (not . isInfixOf tag)

-- Remove whitespace
removeWhiteSpace :: [[Char]] -> [[Char]]
removeWhiteSpace = removeSpaces . removeTabs

-- Remove tabs
removeTabs :: [[Char]] -> [[Char]]
removeTabs xs = map (snd . break (>'\t')) xs

-- Remove spaces
removeSpaces :: [[Char]] -> [[Char]]
removeSpaces xs = map (snd . break (>' ')) xs

-- Strip tags 'tagStart' and 'tagEnd' from [[Char]]
removeTags :: [Char] -> [Char] -> [[Char]] -> [[Char]]
removeTags tagStart tagEnd xs = removeEnd $ removeStart xs
  where
    removeEnd = map $ reverse .snd . splitAt (length tagEnd)
    removeStart = map $ reverse . snd . splitAt (length tagStart)

-- Converts String to Double
stringToDouble :: [Char] -> Double
stringToDouble = read::[Char]->Double

-- Converts Double to Int
doubleToInt :: Double -> Int
doubleToInt = round . (*denormalizer)

-- Generate from a list triples of the list
generateTriples :: [Int] -> [(Int, Int, Int)]
generateTriples (b:g:r:xs) = (r,g,b) : generateTriples xs
generateTriples [] = []

-- Generate from a list triples of the list
generateTriplesAlpha :: [Int] -> [(Int, Int, Int)]
generateTriplesAlpha (a:b:g:r:xs) = (r,g,b) : generateTriplesAlpha xs
generateTriplesAlpha [] = []

-- Convert each channels numeric value to a hex value 
convertColourChannels :: (Int, Int, Int) -> ([Char], [Char], [Char])
convertColourChannels = \(r,g,b) -> (convertHex r, convertHex g, convertHex b)

-- Actual conversion function which calculates for each integer the
-- corresponding hex-value
convertHex:: Int -> [Char]
convertHex 0 = "00"
convertHex colour = (\(x,y) -> hexChar x ++ hexChar y) $ divMod colour 16

-- Combine all channels into a string
toHexColour :: ([Char], [Char], [Char]) -> [Char]
toHexColour = \(r,g,b) -> ("#" ++ r ++ g ++ b)

-- Print function used for st printing
myPrint :: [Char] -> [Char]
myPrint str = "\"" ++ str ++ "\","

-- Print function used for st printing
stPrintNormal :: [Char] -> [Char]
stPrintNormal str = "\t" ++ myPrint str

-- Print function used for st printing
stPrintNumbered :: (Int, [Char]) -> [Char]
stPrintNumbered (idx, str) = "\t[" ++ show idx ++ "] = " ++ myPrint str

-- Print function used for xterm printing
xtPrintNormal :: (Int, [Char]) -> [Char]
xtPrintNormal (idx, str) = "*color" ++ show idx ++ ": " ++ str

-- Print function used for xterm printing
xtPrintSpecial :: ([Char], [Char]) -> [Char]
xtPrintSpecial (idx, str) = "*" ++ idx ++ ": " ++ str

-- Decide how to print our converted values
printColourScheme :: [Char] -> [(Int, [Char])] -> [[Char]] -> IO ()
printColourScheme format pairs colours
  | format == "st"    = printSt pairs colours
  | format == "xterm" = printXterm pairs colours
  | otherwise         = error "Unknown format"

-- Processes the lines passed by 'file' and returns a list of rgb-values as strings
--convertToHex :: Monad m => [[Char]] -> m [[Char]]
convertToHex file = do
  let reals = map stringToDouble $ removeTags realStart realEnd $ removeWhiteSpace $ getFiltered realStart file
  if ((length reals) > (4*23))
    then do
      let rgb = map toHexColour $ map convertColourChannels $ generateTriplesAlpha $ map doubleToInt reals
      return rgb
    else do
      let rgb = map toHexColour $ map convertColourChannels $ generateTriples $ map doubleToInt reals
      return rgb

-- Processes the lines passed by 'file' and returns a list of colour-ids
extractValues :: Monad m => [[Char]] -> m [[Char]]
extractValues file = do
  let keys = removeTags keyStart keyEnd $ removeFiltered colspace . removeFiltered component . removeWhiteSpace $ getFiltered keyStart file
  return keys

-- Prints the values in the default style of st's config.def.h
printSt :: [(Int, [Char])] -> [[Char]] -> IO ()
printSt pairs colours = do
  putStrLn("static const char *colorname[]= {")
  putStrLn("\t/* 8 normal colors */")
  mapM_ (putStrLn . stPrintNormal) $ take 8 colours
  putStrLn("")
  putStrLn("\t/* 8 bright colors */")
  mapM_ (putStrLn . stPrintNormal) $ take 8 $ drop 8 colours
  putStrLn("")
  putStrLn("\t[255] = 0,")
  putStrLn("")
  putStrLn("\t/* more colors can be added after 255 to use with DefaultXX */")
  mapM_ (putStrLn . stPrintNumbered) $ take 4 $ drop 16 pairs
  putStrLn("};")
  putStrLn("")
  putStrLn("/*")
  putStrLn(" * Default colors (colorname index)")
  putStrLn(" * foreground, background, cursor, reverse cursor")
  putStrLn(" */")
  putStrLn("static unsigned int defaultfg = 257;")
  putStrLn("static unsigned int defaultbg = 256;")
  putStrLn("static unsigned int defaultcs = 258;")
  putStrLn("static unsigned int defaultrcs = 259;")

-- Prints the values in the default style of a .Xressource file
printXterm :: [(Int, [Char])] -> [[Char]] -> IO ()
printXterm pairs colours = do
  mapM_ (putStrLn . xtPrintNormal ) $ take 16 pairs
  mapM_ (putStrLn . xtPrintSpecial) $ toList $ delete "cursorTextColor" $ fromList $ zip ["background","foreground","cursorColor","cursorTextColor","highlightTextColor","highlightColor"] $ take 6 $ drop 16 colours

main :: IO ()
main = do
  -- Process input and map it to corresponding variables
  argv <- getArgs
  let argc = length argv
  when (argc /= 2) $ error "Invalid number of arguments"
  let file = head argv
  let format = last argv
  --file <- getLine
  --format <- getLine

  -- Read the file and generate both lists (rgb colour codes and colour-ids
  content <- getLines file
  values <- convertToHex content
  keys <- extractValues content

  -- Zip up both lists and sort them for prettier printing
  let pairs = assocs $ mapKeys (keyConvert) (fromList (zip keys values))
  let colours = elems $ fromList pairs

  --let pairs = assocs $ fromList $ zip keys values
  --mapM_ (putStrLn . (\(x,y) -> x ++ ": " ++ y)) $ pairs

  -- Print colourscheme based on format
  printColourScheme format pairs colours
