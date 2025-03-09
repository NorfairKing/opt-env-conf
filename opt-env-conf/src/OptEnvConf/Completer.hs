module OptEnvConf.Completer
  ( Completer (..),
    filePath,
    directoryPath,
  )
where

import Data.List
import Path
import Path.IO
import qualified System.FilePath as FP

newtype Completer = Completer {unCompleter :: IO [String]}

filePath :: Completer
filePath = Completer $ do
  here <- getCurrentDir
  (ds, fs) <- listDirRel here
  pure $
    filter (not . ("." `isPrefixOf`)) (map fromRelFile fs)
      ++ map (FP.dropTrailingPathSeparator . fromRelDir) ds

directoryPath :: Completer
directoryPath = Completer $ do
  here <- getCurrentDir
  map (FP.dropTrailingPathSeparator . fromRelDir) . fst <$> listDirRel here
