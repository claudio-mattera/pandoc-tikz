
module Text.Pandoc.TikZ.Internal where

import Text.Pandoc

import Text.Pandoc.Walk (query, walk)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Writers.LaTeX (writeLaTeX)

import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Class (lift)

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

compileLatexSource :: LatexSource -> ExceptT String IO BS.ByteString
compileLatexSource (LatexSource source) = do
  let document = Pandoc nullMeta [RawBlock (Format "latex") source]
      options = def { writerStandalone = True
                    , writerTemplate = template
                    }
  output <- lift $ makePDF "pdflatex" writeLaTeX options document
  case output of
    Left msg -> throwE $ BS.unpack msg
    Right output -> return output

compileLatexSourceToFile :: LatexSource -> FilePath -> ExceptT String IO ()
compileLatexSourceToFile source filename = do
  rawPdf <- compileLatexSource source
  lift $ BS.writeFile filename rawPdf
  return ()

template :: String
template = "\\documentclass{standalone}\n" ++
           "\\usepackage{tikz}\n" ++
           "\\usepackage{pgfplots}\n" ++
           "\\begin{document}\n" ++
           "$body$\n" ++
           "\\end{document}\n"
