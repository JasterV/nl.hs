module Lib
  ( numberLines,
    numberAllLines,
    numberNonEmptyLines,
    numberAndIncrNonEmptyLines,
    isEmpty,
    isNotEmpty,
    pad,
    PadMode (..),
    NumberedLine,
    NumberedLines,
    prettyNumberedLines,
  )
where

import Data.Char
import Prelude hiding (lines)

type NumberedLine = (Maybe Int, String)

type NumberedLines = [NumberedLine]

data PadMode = PadLeft | PadRight | PadCenter

numberAllLines :: [String] -> NumberedLines
numberAllLines = numberLines (const True) (const True)

numberNonEmptyLines :: [String] -> NumberedLines
numberNonEmptyLines = numberLines (const True) isNotEmpty

numberAndIncrNonEmptyLines :: [String] -> NumberedLines
numberAndIncrNonEmptyLines = numberLines isNotEmpty isNotEmpty

numberLines :: (String -> Bool) -> (String -> Bool) -> [String] -> NumberedLines
numberLines shouldIncr shouldNumber lines =
  let go :: Int -> [String] -> NumberedLines
      go _ [] = []
      go counter (x : xs)
        | shouldIncr x && shouldNumber x = (Just counter, x) : go (counter + 1) xs
        | shouldIncr x = (Nothing, x) : go (counter + 1) xs
        | shouldNumber x = (Just counter, x) : go counter xs
        | otherwise = (Nothing, x) : go counter xs
   in go 1 lines

prettyNumberedLines :: PadMode -> NumberedLines -> [String]
prettyNumberedLines mode numberedLines =
  let (numbers, text) = unzip numberedLines
      numberStrings = map (maybe "" show) numbers
      maxLength = maximum $ map length numberStrings
      paddedNumbers = map (pad mode maxLength) numberStrings
   in zipWith (\n s -> n ++ " " ++ s) paddedNumbers text

pad :: PadMode -> Int -> String -> String
pad mode n str =
  let diff = n - length str
      padding = replicate diff ' '
      halfPadding = replicate (diff `div` 2) ' '
   in case mode of
        PadLeft -> padding ++ str
        PadRight -> str ++ padding
        PadCenter -> halfPadding ++ str ++ halfPadding

isEmpty :: String -> Bool
isEmpty str =
  null str || all (\s -> not (isPrint s) || isSeparator s) str

isNotEmpty :: String -> Bool
isNotEmpty = not . isEmpty
