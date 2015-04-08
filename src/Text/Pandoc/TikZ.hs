
module Text.Pandoc.TikZ where

import Text.Pandoc
import Text.Pandoc.Walk (query, walk)

newtype LatexSource = LatexSource String
                      deriving (Eq, Show)

extractLatexSource :: Block -> [LatexSource]
extractLatexSource (RawBlock (Format "latex") source) = [LatexSource source]
extractLatexSource _ = []

extractLatexSources :: Pandoc -> [LatexSource]
extractLatexSources = query extractLatexSource

replaceLatexSourceWithHashImage :: Block -> Block
replaceLatexSourceWithHashImage = undefined

replaceLatexSourceWithHashImages :: Pandoc -> Pandoc
replaceLatexSourceWithHashImages = walk replaceLatexSourceWithHashImage

readDoc :: String -> Pandoc
readDoc = readMarkdown def
