module OptEnvConf.Completer
  ( Completer (..),
    filePath,
    directoryPath,
  )
where

import Path
import Path.IO

newtype Completer = Completer {unCompleter :: IO [String]}

filePath :: Completer
filePath = Completer $ do
  here <- getCurrentDir
  map fromAbsFile . snd <$> listDir here

directoryPath :: Completer
directoryPath = Completer $ do
  here <- getCurrentDir
  map fromAbsDir . fst <$> listDir here
