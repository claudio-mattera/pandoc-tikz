
module Text.Pandoc.TikZ (readDoc, writeDoc, processDocument) where

import Text.Pandoc

import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)

import System.FilePath ((</>), (<.>))

import Text.Pandoc.TikZ.Hash (hash)
import Text.Pandoc.TikZ.GhostScript (convertPDFtoPNG)
import Text.Pandoc.TikZ.Internal

readDoc :: String -> Pandoc
readDoc = readMarkdown def

writeDoc :: Pandoc -> String
writeDoc = writeMarkdown def

processDocument :: FilePath -> FilePath -> Pandoc -> IO Pandoc
processDocument ghostScriptPath outputDirectory document = do
  let outputDocument = replaceLatexSourceWithHashImages document
      latexSources = extractLatexSources document

  outcomes <- mapM (runExceptT . compileLatexToPng) latexSources
  mapM_ handleOutcomes outcomes

  return outputDocument
  where
    compileLatexToPng :: LatexSource -> ExceptT String IO ()
    compileLatexToPng source@(LatexSource raw) = do
      let h = hash raw
          pdfFilePath = outputDirectory </> h <.> "pdf"
          pngFilePath = outputDirectory </> h <.> "png"
      compileLatexSourceToFile source pdfFilePath
      convertPDFtoPNG ghostScriptPath pdfFilePath pngFilePath

    handleOutcomes (Right _) = return ()
    handleOutcomes (Left message) = putStrLn $ "Error:\n" ++ message
