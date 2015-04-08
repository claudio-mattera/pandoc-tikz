
module Text.Pandoc.TikZ where

import Text.Pandoc
import Text.Pandoc.Walk (query)

extractLatexSource :: Block -> [String]
extractLatexSource (RawBlock (Format "latex") source) = [source]
extractLatexSource _ = []

extractLatexSources :: Pandoc -> [String]
extractLatexSources = query extractLatexSource

readDoc :: String -> Pandoc
readDoc = readMarkdown def
