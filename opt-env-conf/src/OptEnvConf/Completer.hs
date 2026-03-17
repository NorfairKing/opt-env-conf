{-# LANGUAGE LambdaCase #-}

module OptEnvConf.Completer
  ( Completer (..),
    mkCompleter,
    CompletionFinality (..),
    CompletionResult (..),
    finalResult,
    listCompleter,
    listIOCompleter,
    filePath,
    filePathWithExtension,
    filePathWithExtensions,
    directoryPath,
  )
where

import Data.List
import Data.Maybe
import Path
import Path.IO

-- | Whether a shell should consider a completion result to be complete.
--
-- A final result like @file.txt@ means the user is done typing this
-- argument, so the shell should append a trailing space.
--
-- A non-final result like @dir/@ means the user likely wants to keep
-- typing (e.g. to complete a file inside the directory), so the shell
-- should not append a trailing space.
data CompletionFinality
  = -- | The completion is complete; the shell should append a trailing space.
    CompletionFinal
  | -- | The completion may be extended further; no trailing space.
    CompletionNotFinal
  deriving (Show, Eq, Ord)

data CompletionResult = CompletionResult
  { completionResultValue :: !String,
    completionResultFinality :: !CompletionFinality
  }
  deriving (Show, Eq, Ord)

finalResult :: String -> CompletionResult
finalResult s =
  CompletionResult
    { completionResultValue = s,
      completionResultFinality = CompletionFinal
    }

newtype Completer = Completer {unCompleter :: String -> IO [CompletionResult]}

-- Forward-compatible synonym for the 'Completer' constructor
mkCompleter :: (String -> IO [CompletionResult]) -> Completer
mkCompleter = Completer

listCompleter :: [String] -> Completer
listCompleter ss = listIOCompleter $ pure ss

listIOCompleter :: IO [String] -> Completer
listIOCompleter act = Completer $ \s -> filterPrefix s . map finalResult <$> act

filePath :: Completer
filePath = Completer $ \fp' -> do
  here <- getCurrentDir

  -- An empty string is not a valid relative file or dir, but it is the most
  -- common option so we special case it here
  let (prefix, fp) = stripCurDir fp'
  fmap (filterPrefix fp' . map (addPrefix prefix)) $ do
    let listDirForgiving d = fromMaybe ([], []) <$> forgivingAbsence (listDirRel d)
    (dirsFromParentListing, filesFromParentListing) <- case parseSomeDir fp of
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

    (dirsFromPartialListing, filesFromPartialListing) <- case parseSomeFile fp of
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
        [ map fileResult filesFromPartialListing,
          map fileResult filesFromParentListing,
          map dirResult dirsFromPartialListing,
          map dirResult dirsFromParentListing
        ]
  where
    addPrefix :: String -> CompletionResult -> CompletionResult
    addPrefix pfx cr = cr {completionResultValue = pfx <> completionResultValue cr}

filePathWithExtension :: String -> Completer
filePathWithExtension ext = filePathWithExtensions [ext]

filePathWithExtensions :: [String] -> Completer
filePathWithExtensions exts = Completer $ \s -> do
  results <- unCompleter filePath s
  pure $ filter matchesExtension results
  where
    matchesExtension cr
      | "/" `isSuffixOf` completionResultValue cr = True
      | otherwise = any (`isSuffixOf` completionResultValue cr) exts

directoryPath :: Completer
directoryPath = Completer $ \fp' -> do
  here <- getCurrentDir

  -- An empty string is not a valid relative file or dir, but it is the most
  -- common option so we special case it here
  let (prefix, fp) = stripCurDir fp'
  fmap (filterPrefix fp' . map (addPrefix prefix . dirResult)) $ do
    let listDirForgiving d = fromMaybe ([], []) <$> forgivingAbsence (listDirRel d)
    dirsFromParentListing <- case parseSomeDir fp of
      Nothing -> case fp of
        [] -> do
          -- This is not a valid rel dir but still a prefix of a valid rel dir:
          -- the current dir
          (ds, _) <- listDirRel here
          pure (map fromRelDir $ filter (not . hiddenRel) ds)
        _ -> pure []
      Just (Abs ad) -> do
        (ds, _) <- listDirForgiving ad
        pure (map (fromAbsDir . (ad </>)) $ filter (not . hiddenRel) ds)
      Just (Rel rd) -> do
        (ds, _) <- listDirForgiving rd
        pure (map (fromRelDir . (rd </>)) $ filter (not . hiddenRel) ds)

    dirsFromPartialListing <- case parseSomeDir fp of
      Nothing -> pure []
      Just (Abs af) -> do
        let dir = parent af
        let filterHidden = if hiddenRel (dirname af) then id else filter (not . hiddenRel)
        (ds, _) <- listDirForgiving dir
        pure (map (fromAbsDir . (dir </>)) $ filterHidden ds)
      Just (Rel rf) ->
        -- This is not a valid rel dir but still a prefix of a valid
        -- (hidden) rel dir.
        if fp == "."
          then do
            (ds, _) <- listDirRel here
            pure (map fromRelDir ds)
          else do
            let dir = parent rf
            let filterHidden = if hiddenRel rf then id else filter (not . hiddenRel)
            (ds, _) <- listDirForgiving dir
            pure (map (fromRelDir . (dir </>)) $ filterHidden ds)

    pure $
      concat
        [ dirsFromPartialListing,
          dirsFromParentListing
        ]
  where
    addPrefix :: String -> CompletionResult -> CompletionResult
    addPrefix pfx cr = cr {completionResultValue = pfx <> completionResultValue cr}

fileResult :: String -> CompletionResult
fileResult s =
  CompletionResult
    { completionResultValue = s,
      completionResultFinality = CompletionFinal
    }

dirResult :: String -> CompletionResult
dirResult s =
  CompletionResult
    { completionResultValue = s,
      completionResultFinality = CompletionNotFinal
    }

hiddenRel :: Path Rel f -> Bool
hiddenRel p = case toFilePath p of
  ('.' : _) -> True
  _ -> False

stripCurDir :: FilePath -> (FilePath, FilePath)
stripCurDir = \case
  '.' : '/' : rest' ->
    let (pf, rest) = stripCurDir rest'
     in ("./" <> pf, rest)
  p -> ("", p)

filterPrefix :: String -> [CompletionResult] -> [CompletionResult]
filterPrefix s = filter ((s `isPrefixOf`) . completionResultValue)
