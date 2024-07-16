{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module OptEnvConf.Args
  ( -- * Public API
    Args (..),
    emptyArgs,
    argsLeftovers,
    parseArgs,
    consumeArgument,
    consumeOption,
    consumeSwitch,

    -- ** Internals
    Tomb (..),
    Arg (..),
    parseArg,
    renderArg,
    Dashed (..),
    renderDashed,
    renderDashedArg,
    prefixDashed,
  )
where

import Control.Arrow
import Control.Monad
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.String
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)
import GHC.IsList

-- | Tombstone for leftovers of consumed arguments
data Tomb a
  = -- | Consumed
    Dead
  | -- | Unconsumed
    Live a
  deriving (Show, Eq, Generic)

instance (Validity a) => Validity (Tomb a)

instance (IsString a) => IsString (Tomb a) where
  fromString = Live . fromString

data Arg
  = ArgBareDoubleDash
  | ArgBareDash
  | ArgDashed !Bool !(NonEmpty Char) -- True means long
  | ArgPlain !String
  deriving (Show, Eq, Generic)

instance Validity Arg where
  validate arg =
    mconcat
      [ genericValidate arg,
        case arg of
          ArgDashed False (c :| _) -> declare "The first character of a short dashed is not a dash" $ c /= '-'
          ArgPlain s -> declare "does not start with a dash" $ case s of
            ('-' : _) -> False
            _ -> True
          _ -> valid
      ]

instance IsString Arg where
  fromString = parseArg

parseArg :: String -> Arg
parseArg = \case
  '-' : '-' : rest -> case NE.nonEmpty rest of
    Nothing -> ArgBareDoubleDash
    Just ne -> ArgDashed True ne
  '-' : rest -> case NE.nonEmpty rest of
    Nothing -> ArgBareDash
    Just ne -> ArgDashed False ne
  s -> ArgPlain s

renderArg :: Arg -> String
renderArg = \case
  ArgBareDoubleDash -> "--"
  ArgBareDash -> "-"
  ArgDashed l cs -> (if l then "--" else "-") <> NE.toList cs
  ArgPlain a -> a

-- | An abstraction over '[String]' that makes it easier to implement
-- 'consumeArgument', 'consumeOption' and 'consumeSwitch'.
--
-- In order to implement folded short dashed options, we need to use tombstones
-- for consumed argumentsn
data Args = Args
  { argsBefore :: [Tomb Arg],
    argsAfter :: [Tomb Arg]
  }
  deriving (Show, Eq, Generic)

instance Validity Args

instance IsList Args where
  type Item Args = Tomb Arg
  fromList l = Args {argsBefore = [], argsAfter = l}
  toList = rebuildArgs

-- | Empty list of arguments
emptyArgs :: Args
emptyArgs = parseArgs []

rebuildArgs :: Args -> [Tomb Arg]
rebuildArgs Args {..} = argsBefore ++ argsAfter

argsLeftovers :: Args -> Maybe (NonEmpty String)
argsLeftovers =
  NE.nonEmpty
    . mapMaybe
      ( \case
          Live a -> Just (renderArg a)
          Dead -> Nothing
      )
    . rebuildArgs

-- | Create 'Args' with all-live arguments and cursor at the start.
parseArgs :: [String] -> Args
parseArgs args = Args {argsBefore = [], argsAfter = map (Live . parseArg) args}

-- | Consume a single positional argument.
--
-- The result are all possible results
consumeArgument :: Args -> [(Maybe String, Args)]
consumeArgument as = do
  case argsAfter as of
    [] -> [(Nothing, as)]
    (firstArg : afters) ->
      let befores = argsBefore as
          consumed = Args (befores ++ [Dead]) afters
       in case firstArg of
            -- Skip any dead argument
            Dead -> consumeArgument consumed
            Live a -> case a of
              -- Plain argument: that's the only option, consume it.
              ArgPlain plain -> [(Just plain, consumed)]
              -- A single dash is always an argument
              ArgBareDash -> [(Just "-", consumed)]
              -- Bare double-dash
              ArgBareDoubleDash -> case afters of
                -- If it's the last argument, consume it as an argument
                [] -> [(Just "--", consumed)]
                -- If there's only a dead argument after the double dash, that
                -- means we've been parsing bare args and are now done.
                -- We can stop consuming but get rid of the tombstone as well.
                -- Otherwise there will be a leftover unconsumed '--' after all parsing is done.
                [Dead] -> [(Nothing, Args befores [])]
                -- If it's not the last argument, anything after here is an argument.
                -- In order to not have to maintain whether the cursor is after
                -- a bare double dash already, we keep the cursor here and just
                -- pop the args as they come.
                _ ->
                  let go = \case
                        [] -> Nothing
                        (Dead : rest) -> go rest
                        (Live a' : rest) -> Just (a', rest)
                   in case go afters of
                        Nothing -> [(Nothing, as)]
                        Just (firstLive, rest) ->
                          -- We need to leave the dead argument there so that
                          -- we don't consume the double-dash as an argument
                          -- after consuming all the arguments after it as bare
                          -- arguments.
                          [ ( Just $ renderArg firstLive,
                              Args befores (Live ArgBareDoubleDash : Dead : rest)
                            )
                          ]
              ArgDashed {} ->
                -- Dead after dashed, two options, in order that they should be considered:
                --   * The dashed is a switch (don't consume an arg)
                --   * The dashed is an argument
                -- TODO we need to continue looking too
                let switchCase =
                      consumeArgument (Args (befores ++ [firstArg]) afters)
                        ++ [ (Just (renderArg a), consumed)
                           ]
                 in case afters of
                      -- Last argument is is dashed, that's the same as being followed by a dead argument
                      [] -> switchCase
                      (Dead : _) -> switchCase
                      (Live a' : rest) ->
                        -- TODO we need to continue looking too
                        -- Live after dashed, three options, in order that they should be considered:
                        --   * The dashed is an option and the live is the value
                        --   * The dashed is a switch and the live is an argument
                        --   * The dashed is an argument
                        [(Nothing, as)] -- TODO keep looking
                          ++ [ (Just (renderArg a'), Args (befores ++ [Live a, Dead]) rest),
                               (Just (renderArg a), consumed)
                             ]

-- | Consume an option.
--
-- This supports:
--
--     * @["-f", "foo"]@
--     * @["--foo", "foo"]@
--     * @["-df", "foo"]@
--     * @["--foo=foo"]@
--     * @["-ffoo"]@
consumeOption :: [Dashed] -> Args -> Maybe (String, Args)
consumeOption dasheds as = do
  case go (argsBefore as) of
    Just (val, newBefores) -> Just (val, as {argsBefore = newBefores})
    Nothing ->
      -- TODO option value on the border
      case go (argsAfter as) of
        Just (val, newAfters) -> Just (val, as {argsAfter = newAfters})
        Nothing -> Nothing
  where
    go :: [Tomb Arg] -> Maybe (String, [Tomb Arg])
    go = \case
      [] -> Nothing
      -- Skip dead args
      (Dead : rest) -> second (Dead :) <$> go rest
      -- If we find a live key, try to consume it.
      (Live k : rest) ->
        case k of
          -- We can either consume it as-is, or as a shorthand option.
          ArgDashed isLong cs ->
            case consumeDashedShorthandOption dasheds isLong cs of
              Just v -> Just (v, Dead : rest)
              Nothing ->
                case rest of
                  (Live v : rest') ->
                    case consumeDashedOption dasheds isLong cs of
                      Nothing -> second (Live k :) <$> go rest
                      Just Nothing -> Just (renderArg v, Dead : rest')
                      Just (Just cs') -> Just (renderArg v, Live (ArgDashed isLong cs') : Dead : rest')
                  _ -> second (Live k :) <$> go rest
          _ -> second (Live k :) <$> go rest

consumeDashedShorthandOption ::
  [Dashed] ->
  Bool ->
  NonEmpty Char ->
  Maybe String
consumeDashedShorthandOption dasheds isLong cs =
  if isLong
    then consumeLongDashedShorthandOption (longDasheds dasheds) cs
    else consumeShortDashedShorthandOption (shortDasheds dasheds) cs

consumeLongDashedShorthandOption ::
  [NonEmpty Char] ->
  NonEmpty Char ->
  Maybe String
consumeLongDashedShorthandOption dasheds cs =
  msum $
    map
      ( \dashed ->
          stripPrefix
            (NE.toList dashed ++ "=")
            (NE.toList cs)
      )
      dasheds

consumeShortDashedShorthandOption ::
  [Char] ->
  NonEmpty Char ->
  Maybe String
consumeShortDashedShorthandOption dasheds = \case
  (c :| rest)
    | c `elem` dasheds && not (null rest) ->
        Just rest
  _ -> Nothing

-- Can consume only the last in a folded dashed
consumeDashedOption ::
  [Dashed] ->
  Bool ->
  NonEmpty Char ->
  Maybe (Maybe (NonEmpty Char))
consumeDashedOption dasheds isLong cs =
  if isLong
    then
      if DashedLong cs `elem` dasheds
        then Just Nothing
        else Nothing
    else
      let (mRest, c) = unsnocNE cs
       in if DashedShort c `elem` dasheds
            then Just mRest
            else Nothing

unsnocNE :: NonEmpty a -> (Maybe (NonEmpty a), a)
unsnocNE = go []
  where
    go acc ne =
      let (a, mRest) = NE.uncons ne
       in case mRest of
            Nothing -> (NE.nonEmpty $ reverse acc, a)
            Just rest -> go (a : acc) rest

-- | Consume a switch.
--
-- This supports:
--
--     * @["-f"]@
--     * @["--foo"]@
--     * @["-df"]@
consumeSwitch :: [Dashed] -> Args -> Maybe Args
consumeSwitch dasheds as = do
  case go (argsBefore as) of
    Just newBefores -> Just $ as {argsBefore = newBefores}
    Nothing -> case go (argsAfter as) of
      Just newAfters -> Just $ as {argsAfter = newAfters}
      Nothing -> Nothing
  where
    go :: [Tomb Arg] -> Maybe [Tomb Arg]
    go = \case
      [] -> Nothing
      (Dead : rest) -> (Dead :) <$> go rest
      (Live o : rest) -> case o of
        ArgDashed isLong cs -> case consumeDashedSwitch dasheds isLong cs of
          Nothing -> (Live o :) <$> go rest
          Just Nothing -> Just $ Dead : rest
          Just (Just (cs', needTombstone)) ->
            let rest' = if needTombstone then Dead : rest else rest
             in Just $ Live (ArgDashed isLong cs') : rest'
        _ -> do
          os <- go rest
          pure $ Live o : os

-- Can consume anywhere in a folded dashed, return True if it was the last
-- character because then we need a tombstone.
consumeDashedSwitch ::
  [Dashed] ->
  Bool ->
  NonEmpty Char ->
  Maybe (Maybe (NonEmpty Char, Bool))
consumeDashedSwitch dasheds isLong cs =
  if isLong
    then
      if DashedLong cs `elem` dasheds
        then Just Nothing
        else Nothing
    else consumeChar (shortDasheds dasheds) cs

consumeChar :: [Char] -> NonEmpty Char -> Maybe (Maybe (NonEmpty Char, Bool))
consumeChar cs = go
  where
    go :: NonEmpty Char -> Maybe (Maybe (NonEmpty Char, Bool))
    go (c :| rest) =
      if c `elem` cs
        then Just $ (\ne -> (ne, null rest)) <$> NE.nonEmpty rest
        else do
          rest' <- NE.nonEmpty rest
          new <- go rest'
          pure $
            Just $
              maybe
                (c :| [], True)
                (first (c NE.<|))
                new

data Dashed
  = DashedShort !Char
  | DashedLong !(NonEmpty Char)
  deriving (Show, Eq, Generic)

instance Validity Dashed

instance IsString Dashed where
  fromString s = case fromString s of
    ArgDashed True cs -> DashedLong cs
    ArgDashed False (c :| []) -> DashedShort c
    _ -> error "Invalid dashed"

renderDashed :: Dashed -> String
renderDashed = \case
  DashedShort c -> ['-', c]
  DashedLong cs -> '-' : '-' : NE.toList cs

renderDashedArg :: Dashed -> Arg
renderDashedArg = \case
  DashedShort c -> ArgDashed False (c :| [])
  DashedLong cs -> ArgDashed True cs

prefixDashed :: String -> Dashed -> Dashed
prefixDashed p = \case
  DashedLong l -> DashedLong $ p `NE.prependList` l
  DashedShort c -> DashedShort c

shortDasheds :: [Dashed] -> [Char]
shortDasheds =
  mapMaybe
    ( \case
        DashedShort c -> Just c
        DashedLong _ -> Nothing
    )

longDasheds :: [Dashed] -> [NonEmpty Char]
longDasheds =
  mapMaybe
    ( \case
        DashedLong l -> Just l
        DashedShort _ -> Nothing
    )
