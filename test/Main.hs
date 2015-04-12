module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Pandoc.TikZ
import Text.Pandoc.TikZ.Internal

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
          expected = [ LatexSource ("\\begin{tikzpicture}\n" ++
                                    "% Some comment\n" ++
                                    "\\end{tikzpicture}")
                     , LatexSource ("\\begin{tikzpicture}\n" ++
                                    "% Other comment\n" ++
                                    "\\end{tikzpicture}")
                     ]
      in extractLatexSources (readDoc input) @?= expected
  ,
    testCase "Replacing Latex sources with hash" $
      let input = "Title\n" ++
                  "====\n" ++
                  "\n" ++
                  "\\begin{tikzpicture}\n" ++
                  "% Some comment\n" ++
                  "\\end{tikzpicture}\n" ++
                  "\n" ++
                  "Some text\n" ++
                  "\n" ++
                  "    Some code\n" ++
                  "\n" ++
                  "\\begin{tikzpicture}\n" ++
                  "% Other comment\n" ++
                  "\\end{tikzpicture}\n" ++
                  "\n" ++
                  "Simple *text*\n"
          expected = "Title\n" ++
                     "====\n" ++
                     "\n" ++
                     "![](1bb5e364a4171161fc5e8e8690b21bd4b2551e3244d7983e1ddf299d94118b94.png)\n" ++
                     "\n" ++
                     "Some text\n" ++
                     "\n" ++
                     "    Some code\n" ++
                     "\n" ++
                     "![](8c48be1e04cfbe0d64e6a559133a7a4a76e2dec56701357c505d6f28a7d87a9a.png)\n" ++
                     "\n" ++
                     "Simple *text*\n"
      in replaceLatexSourceWithHashImages (readDoc input) @?= readDoc expected
  ]
