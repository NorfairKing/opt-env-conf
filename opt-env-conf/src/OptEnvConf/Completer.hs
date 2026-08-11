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

-- | Complete a file path.
--
-- Files come before directories, each in the order of 'listDirRelSorted'.
filePath :: Completer
filePath = Completer $ \fp' -> do
  here <- getCurrentDir
  let (prefix, fp, baseDir) = splitDotDot here fp'
  filePathFromDir baseDir prefix fp fp'

filePathFromDir :: Path Abs Dir -> String -> FilePath -> FilePath -> IO [CompletionResult]
filePathFromDir baseDir prefix fp fp' = do
  fmap (filterPrefix fp' . map (addPrefix prefix)) $ do
    let listDirForgiving d = fromMaybe ([], []) <$> forgivingAbsence (listDirRelSorted d)
    (dirsFromParentListing, filesFromParentListing) <- case parseSomeDir fp of
      Nothing -> case fp of
        [] -> do
          (ds, fs) <- listDirRelSorted baseDir
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
        let ad = baseDir </> rd
        (ds, fs) <- listDirForgiving ad
        pure
          ( map (fromRelDir . (rd </>)) $ filter (not . hiddenRel) ds,
            map (fromRelFile . (rd </>)) $ filter (not . hiddenRel) fs
          )

    (dirsFromPartialListing, filesFromPartialListing) <- case parseSomeFile fp of
      Nothing ->
        if fp == "."
          then do
            (ds, fs) <- listDirRelSorted baseDir
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
        let ad = baseDir </> dir
        let filterHidden = if hiddenRel rf then id else filter (not . hiddenRel)
        (ds, fs) <- listDirForgiving ad
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

-- | Complete a directory path.
--
-- Results are in the order of 'listDirRelSorted'.
directoryPath :: Completer
directoryPath = Completer $ \fp' -> do
  here <- getCurrentDir
  let (prefix, fp, baseDir) = splitDotDot here fp'
  directoryPathFromDir baseDir prefix fp fp'

directoryPathFromDir :: Path Abs Dir -> String -> FilePath -> FilePath -> IO [CompletionResult]
directoryPathFromDir baseDir prefix fp fp' = do
  fmap (filterPrefix fp' . map (addPrefix prefix . dirResult)) $ do
    let listDirForgiving d = fromMaybe ([], []) <$> forgivingAbsence (listDirRelSorted d)
    dirsFromParentListing <- case parseSomeDir fp of
      Nothing -> case fp of
        [] -> do
          (ds, _) <- listDirRelSorted baseDir
          pure (map fromRelDir $ filter (not . hiddenRel) ds)
        _ -> pure []
      Just (Abs ad) -> do
        (ds, _) <- listDirForgiving ad
        pure (map (fromAbsDir . (ad </>)) $ filter (not . hiddenRel) ds)
      Just (Rel rd) -> do
        let ad = baseDir </> rd
        (ds, _) <- listDirForgiving ad
        pure (map (fromRelDir . (rd </>)) $ filter (not . hiddenRel) ds)

    dirsFromPartialListing <- case parseSomeDir fp of
      Nothing -> pure []
      Just (Abs af) -> do
        let dir = parent af
        let filterHidden = if hiddenRel (dirname af) then id else filter (not . hiddenRel)
        (ds, _) <- listDirForgiving dir
        pure (map (fromAbsDir . (dir </>)) $ filterHidden ds)
      Just (Rel rf) ->
        if fp == "."
          then do
            (ds, _) <- listDirRelSorted baseDir
            pure (map fromRelDir ds)
          else do
            let dir = parent rf
            let ad = baseDir </> dir
            let filterHidden = if hiddenRel rf then id else filter (not . hiddenRel)
            (ds, _) <- listDirForgiving ad
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

-- | 'listDirRel', but sorted.
--
-- The filesystem hands back directory entries in an order it is free to
-- choose, and different filesystems choose differently.
-- Sorting here, at the only place completion looks at a directory, is what
-- makes completion output the same everywhere.
listDirRelSorted :: Path Abs Dir -> IO ([Path Rel Dir], [Path Rel File])
listDirRelSorted d = do
  (ds, fs) <- listDirRel d
  pure (sort ds, sort fs)

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

-- | Split a path at @..@ components.
--
-- Returns @(prefix, remainder, baseDir)@ where:
--
--  * @prefix@ is the literal text up to and including the last @..\/@ (to
--    prepend to completion results)
--  * @remainder@ is the part after that last @..\/@ (to pass to
--    @parseSomeDir@ \/ @parseSomeFile@, which reject @..@)
--  * @baseDir@ is the absolute directory that @remainder@ should be resolved
--    relative to
--
-- If there are no @..@ components, @prefix@ is just the stripped @.\/@ prefix,
-- @remainder@ is the rest, and @baseDir@ is the current directory.
splitDotDot :: Path Abs Dir -> FilePath -> (FilePath, FilePath, Path Abs Dir)
splitDotDot here fp' =
  let (curDirPrefix, afterCurDir) = stripCurDir fp'
      components = splitOnSlash afterCurDir
      -- Find the index after the last "../" component
      lastDotDotIdx = case [i | (i, c) <- zip [1 ..] components, c == ".."] of
        [] -> 0
        ixs -> maximum ixs
   in if lastDotDotIdx == 0
        then (curDirPrefix, afterCurDir, here)
        else
          let prefixComponents = take lastDotDotIdx components
              remainderComponents = drop lastDotDotIdx components
              prefix = curDirPrefix <> concatMap (<> "/") prefixComponents
              -- Preserve trailing slash from the original input
              trailingSlash
                | "/" `isSuffixOf` afterCurDir, not (null remainderComponents) = "/"
                | otherwise = ""
              remainder = intercalate "/" remainderComponents <> trailingSlash
              baseDir = foldl' applyComponent here prefixComponents
           in (prefix, remainder, baseDir)

-- | Split a filepath on @\/@ separators, dropping empty segments.
splitOnSlash :: FilePath -> [String]
splitOnSlash [] = []
splitOnSlash s =
  let (seg, rest) = break (== '/') s
   in case rest of
        [] -> [seg | not (null seg)]
        _ : rest' -> [seg | not (null seg)] <> splitOnSlash rest'

-- | Apply a single path component to an absolute directory.
applyComponent :: Path Abs Dir -> String -> Path Abs Dir
applyComponent d ".." = parent d
applyComponent d c = case parseRelDir c of
  Nothing -> d
  Just rd -> d </> rd

filterPrefix :: String -> [CompletionResult] -> [CompletionResult]
filterPrefix s = filter ((s `isPrefixOf`) . completionResultValue)
