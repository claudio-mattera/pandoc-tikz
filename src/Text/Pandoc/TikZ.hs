
module Text.Pandoc.TikZ where

import Text.Pandoc
import Text.Pandoc.Walk (query, walk)

import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base16 (encode)
import Crypto.Hash.SHA256 (hash)

newtype LatexSource = LatexSource String
                      deriving (Eq, Show)

extractLatexSource :: Block -> [LatexSource]
extractLatexSource (RawBlock (Format "latex") source) = [LatexSource source]
extractLatexSource _ = []

extractLatexSources :: Pandoc -> [LatexSource]
extractLatexSources = query extractLatexSource

replaceLatexSourceWithHashImage :: Block -> Block
replaceLatexSourceWithHashImage (RawBlock (Format "latex") source) =
  Para [Image [] (hashString source ++ ".png", "fig:")]
replaceLatexSourceWithHashImage x = x

replaceLatexSourceWithHashImages :: Pandoc -> Pandoc
replaceLatexSourceWithHashImages = walk replaceLatexSourceWithHashImage

readDoc :: String -> Pandoc
readDoc = readMarkdown def

hashString :: String -> String
hashString = unpack . encode . hash . pack
