module OptEnvConf.Completer
  ( Completer (..),
    listCompleter,
    listIOCompleter,
    filePath,
    directoryPath,
  )
where

import Data.List
import Path
import Path.IO
import qualified System.FilePath as FP

newtype Completer = Completer {unCompleter :: String -> IO [String]}

-- Forward-compatible synonym for the 'Completer' constructor
mkCompleter :: (String -> IO [String]) -> Completer
mkCompleter = Completer

listCompleter :: [String] -> Completer
listCompleter ss = listIOCompleter $ pure ss

listIOCompleter :: IO [String] -> Completer
listIOCompleter act = Completer $ \s -> filter (s `isPrefixOf`) <$> act

filePath :: Completer
filePath = listIOCompleter $ do
  here <- getCurrentDir
  (ds, fs) <- listDirRel here
  pure $
    hideHidden $
      map fromRelFile fs
        ++ map (FP.dropTrailingPathSeparator . fromRelDir) ds

directoryPath :: Completer
directoryPath = listIOCompleter $ do
  here <- getCurrentDir
  hideHidden . map (FP.dropTrailingPathSeparator . fromRelDir) . fst <$> listDirRel here

hideHidden :: [FilePath] -> [FilePath]
hideHidden = filter (not . ("." `isPrefixOf`))
