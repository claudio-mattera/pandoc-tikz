
module Text.Pandoc.TikZ where

import Text.Pandoc
import Text.Pandoc.Walk (query, walk)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Writers.LaTeX (writeLaTeX)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import qualified Data.ByteString.Lazy.Char8 as BS

import Text.Pandoc.TikZ.Hash (hash)
import Text.Pandoc.TikZ.GhostScript (convertPDFtoPNG)

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

writeDoc :: Pandoc -> String
writeDoc = writeMarkdown def

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
  lift $ BS.writeFile (filename ++ ".pdf") rawPdf
  return ()

processDocument :: FilePath -> Pandoc -> IO Pandoc
processDocument ghostScriptPath document = do
  let outputDocument = replaceLatexSourceWithHashImages document
      latexSources = extractLatexSources document

  outcomes <- mapM (runExceptT . compileLatexToPng) latexSources
  mapM_ handleOutcomes outcomes

  return outputDocument
  where
    compileLatexToPng :: LatexSource -> ExceptT String IO ()
    compileLatexToPng source@(LatexSource raw) = do
      let h = hash raw
      compileLatexSourceToFile source h
      convertPDFtoPNG ghostScriptPath (h ++ ".pdf") (h ++ ".png")

    handleOutcomes (Right _) = return ()
    handleOutcomes (Left message) = putStrLn $ "Error:\n" ++ message

template :: String
template = "\\documentclass{standalone}\n" ++
           "\\usepackage{tikz}\n" ++
           "\\usepackage{pgfplots}\n" ++
           "\\begin{document}\n" ++
           "$body$\n" ++
           "\\end{document}\n"
