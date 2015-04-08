
module Text.Pandoc.TikZ.Hash (hash) where

import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base16 (encode)
import qualified Crypto.Hash.SHA256 as SHA256

hash :: String -> String
hash = unpack . encode . SHA256.hash . pack
