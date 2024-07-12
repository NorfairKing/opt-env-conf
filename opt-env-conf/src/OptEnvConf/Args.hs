{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
-- for consumed arguments.
newtype Args = Args
  { unArgs :: [Tomb Arg]
  }
  deriving (Show, Eq, Generic)

instance Validity Args

instance IsList Args where
  type Item Args = Tomb Arg
  fromList = Args
  toList = unArgs

-- | Empty list of arguments
emptyArgs :: Args
emptyArgs = Args []

argsLeftovers :: Args -> Maybe (NonEmpty String)
argsLeftovers =
  NE.nonEmpty
    . mapMaybe
      ( \case
          Live a -> Just (renderArg a)
          Dead -> Nothing
      )
    . unArgs

-- | Create 'Args' with all-live arguments.
parseArgs :: [String] -> Args
parseArgs args = Args $ map (Live . parseArg) args

-- | Consume a single positional argument.
--
-- The result are all possible results
consumeArgument :: Args -> [(Maybe String, Args)]
consumeArgument am = do
  (s, opts') <- go $ unArgs am
  pure (s, am {unArgs = opts'})
  where
    go :: [Tomb Arg] -> [(Maybe String, [Tomb Arg])]
    go = \case
      -- Nothing to consume, return only the option that we consume nothing
      [] -> [(Nothing, [])]
      -- Last argument is dashed (same as dead after dashed below), two options, in order that they should be considered:
      --   * The dashed is a switch (don't consume an arg)
      --   * The dashed is an argument
      [Live o@(ArgDashed _ _)] ->
        [ -- Dashed is a switch, don't consume an arg
          (Nothing, [Live o]),
          -- Dashed is the argument, consume it
          (Just (renderArg o), [Dead])
        ]
      -- Skip dead args
      (Dead : rest) -> map (second (Dead :)) (go rest)
      -- Anything after a "--" should be considered an argument and we can stop looking after that
      (Live ArgBareDoubleDash : rest) -> map (second (Live ArgBareDoubleDash :)) (goBare rest)
      -- A bare dash could be an argument and we can stop looking after that
      -- Impossible to consume nothing in this case
      (Live ArgBareDash : rest) -> [(Just (renderArg ArgBareDash), Dead : rest)]
      -- A plain argument could definitely be an argument and we can stop looking after that
      -- Impossible to consume nothing in this case
      (Live (ArgPlain a) : rest) -> [(Just a, Dead : rest)]
      -- Live after dashed, three options, in order that they should be considered:
      --   * The dashed is an option and the live is the value
      --   * The dashed is a switch and the live is an argument
      --   * The dashed is an argument
      (Live o1@(ArgDashed {}) : Live o2 : rest) ->
        concat
          [ --   -- TODO put this option at the back so it's considered last ?
            -- The dashed is an option and the live is the value
            map (second ((Live o1 :) . (Live o2 :))) (go rest),
            -- The dashed is a switch and the live is an argument
            [(Just (renderArg o2), Live o1 : Dead : rest)],
            -- The dashed is an argument
            [(Just (renderArg o1), Dead : Live o2 : rest)]
          ]
      -- Dead after dashed, two options, in order that they should be considered:
      --   * The dashed is a switch (don't consume an arg)
      --   * The dashed is an argument
      (Live o1@(ArgDashed {}) : Dead : rest) ->
        concat
          [ -- The dashed is a switch, don't consume it as an arg
            map (second (Live o1 :)) (go rest),
            -- The dashed is an argument, consume it
            [(Just (renderArg o1), Dead : rest)]
          ]
    goBare :: [Tomb Arg] -> [(Maybe String, [Tomb Arg])]
    goBare = \case
      -- Nothing to consume, return only the option that we consume nothing
      [] -> [(Nothing, [])]
      -- Skip dead arguments
      (Dead : rest) -> map (second (Dead :)) (goBare rest)
      -- Consume a live argument.
      -- No need to tombstone it because anything after -- can only be an argument.
      -- Impossible to consume nothing in this case
      (Live o : rest) -> [(Just (renderArg o), rest)]

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
consumeOption dasheds am = do
  (mS, opts') <- go $ unArgs am
  pure (mS, am {unArgs = opts'})
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
consumeSwitch dasheds am = do
  opts' <- go $ unArgs am
  pure $ am {unArgs = opts'}
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
