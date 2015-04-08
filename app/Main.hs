
module Main where

import System.Environment (getArgs)

import Text.Pandoc.TikZ (processDocument, readDoc, writeDoc)

main :: IO ()
main = do
  (inputFileName:outputFileName:_) <- getArgs
  inputFile <- readFile inputFileName
  let inputDocument = readDoc inputFile
  outputDocument <- processDocument inputDocument
  writeFile outputFileName . writeDoc $ outputDocument
