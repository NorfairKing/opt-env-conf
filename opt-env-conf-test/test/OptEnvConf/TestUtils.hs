module OptEnvConf.TestUtils (pureGoldenChunksFile) where

import Data.Text (Text)
import Test.Syd
import Text.Colour

pureGoldenChunksFile :: FilePath -> [Chunk] -> GoldenTest Text
pureGoldenChunksFile fp cs =
  pureGoldenTextFile fp $ renderChunksText With24BitColours cs
