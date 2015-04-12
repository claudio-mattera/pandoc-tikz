{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Environment (getArgs, withArgs)
import System.Console.CmdArgs
import System.FilePath (dropExtension, takeFileName, (<.>), (</>))
import System.Directory (createDirectoryIfMissing)
import Data.Maybe (fromMaybe)

import Text.Pandoc.TikZ (processDocument, readDoc, writeDoc)

main :: IO ()
main = do
  args <- getArgs
  Options
    { inputFilePath = inputFilePath
    , outputPath = outputPath
    , maybeGhostScriptPath = maybeGhostScriptPath
    } <- (if null args then withArgs ["--help"] else id) getOptions
  createDirectoryIfMissing True outputPath
  let ghostScriptPath =
        fromMaybe "gs" maybeGhostScriptPath
  inputFile <- readFile inputFilePath
  let inputDocument = readDoc inputFile
  outputDocument <- processDocument ghostScriptPath outputPath inputDocument
  let outputFilePath = outputPath </> takeFileName inputFilePath
  writeFile outputFilePath . writeDoc $ outputDocument

data Options = Options
    { inputFilePath :: FilePath
    , outputPath :: FilePath
    , maybeGhostScriptPath :: Maybe FilePath
    } deriving (Data, Typeable, Show, Eq)

options :: Options
options = Options
    { inputFilePath =
        def
        &= argPos 0
        &= typFile
    , outputPath =
        def
        &= argPos 1
        &= typDir
    , maybeGhostScriptPath =
        def
        &= explicit
        &= name "g"
        &= name "ghostscript"
        &= typFile
        &= help "GhostScript path"
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
