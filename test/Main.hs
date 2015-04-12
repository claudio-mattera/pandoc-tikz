module Main where

import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Lazy.Char8 as BS

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

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
  ,
    let source = LatexSource "\\begin{tikzpicture}\n\\draw (0.1,0) -- (0,0) -- (0,0.1) -- cycle;\n\\end{tikzpicture}"
        worker = do
          result <- runExceptT $ compileLatexSource source
          case result of
            Right output -> return output
            Left _ -> return BS.empty

        expected = "test/4cf7760f6b5cdc55902b25a2693874817a40a2fc2d069bd37a08d4adeceb5dc5.pdf"
    in goldenVsStringDiff "Compile triangle plot" makePdfDiff expected worker
  ,
    let source = LatexSource "\\begin{tikzpicture}\n\\draw (0.1,0) -- (0,0) -- (0,0.1) -- cycle;\n\\end{tikzpicture}"
        outputFileName = expected ++ ".new"
        worker = do
          _ <- runExceptT $ compileLatexSourceToFile source outputFileName
          return ()

        expected = "test/4cf7760f6b5cdc55902b25a2693874817a40a2fc2d069bd37a08d4adeceb5dc5.pdf"
    in goldenVsFileDiff "Compile and write triangle plot" makePdfDiff expected outputFileName worker
  ,
    let source = LatexSource "\\begin{tikzpicture}\n\\ojidoj nk\\lknlkkn\\\\\\lkd f fkn\\\n\\end{tikzpicture}"
        worker = do
          result <- runExceptT $ compileLatexSource source
          case result of
            Right _ -> return BS.empty
            Left output -> return $ BS.pack output

        expected = "test/2202bf79c402467b6a357d3aa679f7af513de5b67d742a8f44dc0439fd856b60.txt"
    in goldenVsStringDiff "Invalid LaTeX" makeTexOutputDiff expected worker
  ]


makePdfDiff ref new = [
    "diff"
  , "--text"
  , "-I"
  , "/ID"
  , "-I"
  , "/CreationDate"
  , "-I"
  , "/ModDate"
  , "-I"
  , "/PTEX.Fullbanner"
  , "-I"
  , "/Producer"
  , "-u"
  , ref
  , new
  ]

makeTexOutputDiff ref new = [
    "diff"
  , "--text"
  , "--ignore-all-space"
  , "--ignore-space-change"
  , "--ignore-blank-lines"
  , "-u"
  , ref
  , new
  ]
