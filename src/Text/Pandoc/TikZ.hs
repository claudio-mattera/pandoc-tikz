
module Text.Pandoc.TikZ where

import Text.Pandoc
import Text.Pandoc.Walk (query, walk)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Writers.LaTeX (writeLaTeX)

import qualified Data.ByteString.Lazy.Char8 as BS

import Text.Pandoc.TikZ.Hash (hash)

newtype LatexSource = LatexSource String
                      deriving (Eq, Show)

extractLatexSource :: Block -> [LatexSource]
extractLatexSource (RawBlock (Format "latex") source) = [LatexSource source]
extractLatexSource _ = []

extractLatexSources :: Pandoc -> [LatexSource]
extractLatexSources = query extractLatexSource

replaceLatexSourceWithHashImage :: Block -> Block
replaceLatexSourceWithHashImage (RawBlock (Format "latex") source) =
  Para [Image [] (hash source ++ ".png", "fig:")]
replaceLatexSourceWithHashImage x = x

replaceLatexSourceWithHashImages :: Pandoc -> Pandoc
replaceLatexSourceWithHashImages = walk replaceLatexSourceWithHashImage

readDoc :: String -> Pandoc
readDoc = readMarkdown def

compileLatexSource :: LatexSource -> IO (Either String BS.ByteString)
compileLatexSource (LatexSource source) = do
  let document = Pandoc nullMeta [RawBlock (Format "latex") source]
      options = def { writerStandalone = True
                    , writerTemplate = template
                    }
  output <- makePDF "pdflatex" writeLaTeX options document
  case output of
    Left msg -> return $ Left $ BS.unpack msg
    Right output -> return $ Right output

compileLatexSourceToFile :: LatexSource -> FilePath -> IO ()
compileLatexSourceToFile source filename = do
  result <- compileLatexSource source
  case result of
    Left msg -> error $ "Got error!!!" ++ msg
    Right rawPdf -> BS.writeFile (filename ++ ".pdf") rawPdf

processDocument :: Pandoc -> IO Pandoc
processDocument document = do
  let outputDocument = replaceLatexSourceWithHashImages document
      latexSources = extractLatexSources document
  mapM_ f latexSources
  return outputDocument
  where
    f source@(LatexSource raw) = compileLatexSourceToFile source (hash raw)

template :: String
template = "\\documentclass{standalone}\n" ++
           "\\usepackage{tikz}\n" ++
           "\\usepackage{pgfplots}\n" ++
           "\\begin{document}\n" ++
           "$body$\n" ++
           "\\end{document}\n"
