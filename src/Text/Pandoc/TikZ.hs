
module Text.Pandoc.TikZ where

import Text.Pandoc
import Text.Pandoc.Walk (query)

newtype LatexSource = LatexSource String
                      deriving (Eq, Show)

extractLatexSource :: Block -> [LatexSource]
extractLatexSource (RawBlock (Format "latex") source) = [LatexSource source]
extractLatexSource _ = []

extractLatexSources :: Pandoc -> [LatexSource]
extractLatexSources = query extractLatexSource

readDoc :: String -> Pandoc
readDoc = readMarkdown def
