module OptEnvConf.Completer
  ( Completer (..),
    mkCompleter,
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
listIOCompleter act = Completer $ \s -> filterPrefix s <$> act

filePath :: Completer
filePath = Completer $ \fp -> do
  here <- getCurrentDir
  (ds, fs) <- listDirRel here
  pure $
    filterPrefix fp $
      hideHiddenIfNoDot fp $
        map fromRelFile fs
          ++ map (FP.dropTrailingPathSeparator . fromRelDir) ds

directoryPath :: Completer
directoryPath = Completer $ \fp -> do
  here <- getCurrentDir
  filterPrefix fp . hideHiddenIfNoDot fp . map (FP.dropTrailingPathSeparator . fromRelDir) . fst <$> listDirRel here

filterPrefix :: String -> [String] -> [String]
filterPrefix s = filter (s `isPrefixOf`)

hideHiddenIfNoDot :: FilePath -> [FilePath] -> [FilePath]
hideHiddenIfNoDot f = case f of
  '.' : _ -> id
  _ -> hideHidden

hideHidden :: [FilePath] -> [FilePath]
hideHidden = filter (not . ("." `isPrefixOf`))
