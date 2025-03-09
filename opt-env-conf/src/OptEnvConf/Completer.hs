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
  (ds, fs) <- listDirRel here
  pure $ map fromRelFile fs ++ map fromRelDir ds

directoryPath :: Completer
directoryPath = Completer $ do
  here <- getCurrentDir
  map fromRelDir . fst <$> listDirRel here
