{-# LANGUAGE LambdaCase #-}

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
import Data.Maybe
import Path
import Path.IO
import Path.Internal.Posix (Path (..))
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
filePath = Completer $ \fp' -> do
  putStrLn ""
  here <- getCurrentDir

  -- An empty string is not a valid relative file or dir, but it is the most
  -- common option so we special case it here
  let (prefix, fp) = stripCurDir fp'
  fmap (filterPrefix fp' . map (prefix <>)) $ do
    let listDirForgiving d = fromMaybe ([], []) <$> forgivingAbsence (listDirRel d)
    (dirsFromDirListing, filesFromDirListing) <- case parseSomeDir fp of
      Nothing -> case fp of
        [] -> do
          -- This is not a valid rel dir but still a prefix of a valid rel dir:
          -- the current dir
          (ds, fs) <- listDirRel here
          pure
            ( map fromRelDir $ filter (not . hiddenRel) ds,
              map fromRelFile $ filter (not . hiddenRel) fs
            )
        _ -> pure ([], [])
      Just (Abs ad) -> do
        (ds, fs) <- listDirForgiving ad
        pure
          ( map (fromAbsDir . (ad </>)) $ filter (not . hiddenRel) ds,
            map (fromAbsFile . (ad </>)) $ filter (not . hiddenRel) fs
          )
      Just (Rel rd) -> do
        (ds, fs) <- listDirForgiving rd
        pure
          ( map (fromRelDir . (rd </>)) $ filter (not . hiddenRel) ds,
            map (fromRelFile . (rd </>)) $ filter (not . hiddenRel) fs
          )

    (dirsFromFileListing, filesFromFileListing) <- case parseSomeFile fp of
      Nothing ->
        -- This is not a valid rel file but still a prefix of a valid
        -- (hidden) rel file.
        if fp == "."
          then do
            (ds, fs) <- listDirRel here
            pure
              ( map fromRelDir ds,
                map fromRelFile fs
              )
          else pure ([], [])
      Just (Abs af) -> do
        let dir = parent af
        let filterHidden = if hiddenRel (filename af) then id else filter (not . hiddenRel)
        (ds, fs) <- listDirForgiving dir
        pure
          ( map (fromAbsDir . (dir </>)) $ filterHidden ds,
            map (fromAbsFile . (dir </>)) $ filterHidden fs
          )
      Just (Rel rf) -> do
        let dir = parent rf
        let filterHidden = if hiddenRel rf then id else filter (not . hiddenRel)
        (ds, fs) <- listDirForgiving dir
        pure
          ( map (fromRelDir . (dir </>)) $ filterHidden ds,
            map (fromRelFile . (dir </>)) $ filterHidden fs
          )

    pure $
      concat
        [ filesFromFileListing,
          filesFromDirListing,
          dirsFromFileListing,
          dirsFromDirListing
        ]

both :: (a -> b) -> (a, a) -> (b, b)
both f (a1, a2) = (f a1, f a2)

directoryPath :: Completer
directoryPath = Completer $ \fp -> do
  here <- getCurrentDir
  filterCurDirPrefix fp . map fromRelDir . fst <$> listDirRel here

hideHiddenIfNoDot :: FilePath -> [FilePath] -> [FilePath]
hideHiddenIfNoDot f = case f of
  '.' : _ -> id
  _ -> hideHidden

hideHidden :: [FilePath] -> [FilePath]
hideHidden = filter (not . ("." `isPrefixOf`))

hiddenRel :: Path Rel f -> Bool
hiddenRel (Path s) = case s of
  ('.' : _) -> True
  _ -> False

filterCurDirPrefix :: FilePath -> [FilePath] -> [FilePath]
filterCurDirPrefix fp' =
  let (fp, prefix) = stripCurDir fp'
   in map (prefix <>) . filterPrefix fp . hideHiddenIfNoDot fp

stripCurDir :: FilePath -> (FilePath, FilePath)
stripCurDir = \case
  '.' : '/' : rest' ->
    let (pf, rest) = stripCurDir rest'
     in ("./" <> pf, rest)
  p -> ("", p)

filterPrefix :: String -> [String] -> [String]
filterPrefix s = filter (s `isPrefixOf`)
