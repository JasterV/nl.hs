module Main (main) where

import Data.Maybe (catMaybes)
import Lib (PadMode (..), numberAllLines, numberAndIncrNonEmptyLines, numberNonEmptyLines, prettyNumberedLines)
import System.Environment

data LineNumberOption
  = ReverseNumbering
  | SkipEmptyLines
  | IgnoreEmptyLines
  | LeftAlign
  | CenterAlign
  | Help
  deriving (Eq)

data Args
  = Args FilePath [LineNumberOption]
  | InvalidOptions [String]
  | NoFilePath

parseLineNumberOption :: String -> Maybe LineNumberOption
parseLineNumberOption "--help" = Just Help
parseLineNumberOption "--reverse" = Just ReverseNumbering
parseLineNumberOption "--skip-empty" = Just SkipEmptyLines
parseLineNumberOption "--ignore-empty" = Just IgnoreEmptyLines
parseLineNumberOption "--left-align" = Just LeftAlign
parseLineNumberOption "--center-align" = Just CenterAlign
parseLineNumberOption _ = Nothing

parseArguments :: [String] -> Args
parseArguments xs = case reverse xs of
  [] -> NoFilePath
  (filename : options) ->
    let parsedOptions = map parseLineNumberOption options
     in if Nothing `elem` parsedOptions
          then InvalidOptions options
          else Args filename (catMaybes parsedOptions)

printHelp :: String -> IO ()
printHelp msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " [--reverse | --left-align | --center-align | --skip-empty | --ignore-empty ] <filename>")

readLines :: FilePath -> IO [String]
readLines path = do
  text <- readFile path
  return (lines text)

main :: IO ()
main = do
  rawArgs <- getArgs
  let args = parseArguments rawArgs

  case args of
    NoFilePath -> printHelp "Missing filename"
    InvalidOptions options -> printHelp ("Invalid arguments: " ++ show options)
    Args _ options | Help `elem` options -> printHelp ""
    Args filepath options -> do
      let numberFn
            | IgnoreEmptyLines `elem` options = numberAndIncrNonEmptyLines
            | SkipEmptyLines `elem` options = numberNonEmptyLines
            | otherwise = numberAllLines

      let padMode
            | LeftAlign `elem` options = PadRight
            | CenterAlign `elem` options = PadCenter
            | otherwise = PadLeft

      fileLines <- readLines filepath

      let numbered = numberFn fileLines
          prettyNumbered = prettyNumberedLines padMode numbered
          revNumbered = numberFn (reverse fileLines)
          revPrettyNumbered = reverse $ prettyNumberedLines padMode revNumbered

      mapM_
        putStrLn
        ( if ReverseNumbering `elem` options
            then revPrettyNumbered
            else prettyNumbered
        )
