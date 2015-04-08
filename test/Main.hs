module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Pandoc.TikZ

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
  [
    testCase "Extracting Latex sources" $
      let input = "Title\n" ++
                  "====\n" ++
                  "\n" ++
                  "\\begin{tikzpicture}\n" ++
                  "% Some comment\n" ++
                  "\\end{tikzpicture}\n" ++
                  "\n" ++
                  "    # Some code\n" ++
                  "\n" ++
                  "\\begin{tikzpicture}\n" ++
                  "% Other comment\n" ++
                  "\\end{tikzpicture}\n" ++
                  "\n" ++
                  "Simple *text*\n"
          expected = [ "\\begin{tikzpicture}\n" ++
                       "% Some comment\n" ++
                       "\\end{tikzpicture}"
                     , "\\begin{tikzpicture}\n" ++
                       "% Other comment\n" ++
                       "\\end{tikzpicture}"]
      in extractLatexSources (readDoc input) @?= expected
  ]
