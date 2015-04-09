{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Environment (getArgs, withArgs)
import System.Console.CmdArgs
import System.FilePath (dropExtension)
import Data.Maybe (fromMaybe)

import Text.Pandoc.TikZ (processDocument, readDoc, writeDoc)

main :: IO ()
main = do
  args <- getArgs
  Options
    { inputFilePath = inputFilePath
    , maybeOutputFilePath = maybeOutputFilePath
    } <- (if null args then withArgs ["--help"] else id) getOptions
  let outputFilePath =
        fromMaybe (defaultOutputFilePath inputFilePath) maybeOutputFilePath
  inputFile <- readFile inputFilePath
  let inputDocument = readDoc inputFile
  outputDocument <- processDocument inputDocument
  writeFile outputFilePath . writeDoc $ outputDocument

  where
    defaultOutputFilePath inputFilePath =
      dropExtension inputFilePath ++ "-output.md"

data Options = Options
    { inputFilePath :: FilePath
    , maybeOutputFilePath :: Maybe FilePath
    } deriving (Data, Typeable, Show, Eq)

options :: Options
options = Options
    { inputFilePath =
        def
        &= args
        &= typFile
    , maybeOutputFilePath =
        def
        &= explicit
        &= name "o"
        &= name "output"
        &= typFile
        &= help "Output file path"
    }

getOptions :: IO Options
getOptions = cmdArgs $ options
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "pandoc-tikz"
_PROGRAM_VERSION = "0.1.0.0"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "A script to embed TikZ pictures in Markdown documents"
_COPYRIGHT = "(C) Claudio Mattera 2014-2015"
