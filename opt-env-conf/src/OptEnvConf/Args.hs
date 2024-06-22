{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module OptEnvConf.Args
  ( Args (..),
    emptyArgs,
    parseArgs,
    consumeArgument,
    consumeOption,
    consumeSwitch,
    Arg (..),
    parseArg,
    renderArg,
    unfoldArg,
    Dashed (..),
    renderDashed,
    renderDashedArg,
    prefixDashed,
    unfoldDasheds,
  )
where

import Control.Applicative
import Control.Arrow
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.String
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)
import GHC.IsList

newtype Args = Args
  { unArgs :: [Arg]
  }
  deriving (Show, Eq, Generic)

instance Validity Args

instance IsList Args where
  type Item Args = Arg
  fromList = Args
  toList = unArgs

emptyArgs :: Args
emptyArgs = Args []

parseArgs :: [String] -> Args
parseArgs args = Args $ map parseArg args

consumeArgument :: Args -> [(String, Args)]
consumeArgument am = do
  (s, opts') <- go $ unArgs am
  pure (s, am {unArgs = opts'})
  where
    go :: [Arg] -> [(String, [Arg])]
    go = \case
      -- Nothing to consume
      [] -> empty
      -- If the last arg is a "--", it could be an argument, I guess
      -- Every arg could be the argument we consume.
      [o] -> [(renderArg o, [])]
      -- Anything after a "--" should be considered an argument
      (ArgBareDoubleDash : o : rest) -> [(renderArg o, ArgBareDoubleDash : rest)]
      -- A bare dash could be an argument
      (ArgBareDash : rest) -> [(renderArg ArgBareDash, rest)]
      -- Any argument after a dashed argument could be an option value so we
      -- should also keep looking after that.
      (o1@(ArgDashed {}) : o2 : rest) ->
        -- TODO put this option at the back so it's considered last ?
        (renderArg o1, o2 : rest)
          : (renderArg o2, o1 : rest)
          : map (second ((o1 :) . (o2 :))) (go rest)
      -- A plain argument could definitely be an argument.
      (ArgPlain a : rest) -> [(a, rest)]

consumeOption :: [Dashed] -> Args -> Maybe (String, Args)
consumeOption dasheds am = do
  (mS, opts') <- go $ unArgs am
  pure (mS, am {unArgs = opts'})
  where
    go :: [Arg] -> Maybe (String, [Arg])
    go = \case
      [] -> Nothing
      [_] -> Nothing
      (k : v : rest) -> case k of
        ArgDashed isLong cs -> case consumeDashedOption dasheds isLong cs of
          Nothing -> second (k :) <$> go (v : rest)
          Just Nothing -> Just (renderArg v, rest)
          Just (Just cs') -> Just (renderArg v, ArgDashed isLong cs' : rest)
        _ -> do
          (mS, as) <- go (v : rest)
          pure (mS, k : as)

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

consumeSwitch :: [Dashed] -> Args -> Maybe Args
consumeSwitch dasheds am = do
  opts' <- go $ unArgs am
  pure $ am {unArgs = opts'}
  where
    go :: [Arg] -> Maybe [Arg]
    go = \case
      [] -> Nothing
      (o : rest) -> case o of
        ArgDashed isLong cs -> case consumeDashedSwitch dasheds isLong cs of
          Nothing -> (o :) <$> go rest
          Just Nothing -> Just rest
          Just (Just cs') -> Just $ ArgDashed isLong cs' : rest
        _ -> do
          os <- go rest
          pure $ o : os

-- Can consume anywhere in a folded dashed
consumeDashedSwitch ::
  [Dashed] ->
  Bool ->
  NonEmpty Char ->
  Maybe (Maybe (NonEmpty Char))
consumeDashedSwitch dasheds isLong cs =
  if isLong
    then
      if DashedLong cs `elem` dasheds
        then Just Nothing
        else Nothing
    else
      let shorts =
            mapMaybe
              ( \case
                  DashedShort c -> Just c
                  DashedLong _ -> Nothing
              )
              dasheds
       in consumeChar shorts cs

consumeChar :: [Char] -> NonEmpty Char -> Maybe (Maybe (NonEmpty Char))
consumeChar cs = go
  where
    go :: NonEmpty Char -> Maybe (Maybe (NonEmpty Char))
    go (c :| rest) =
      if c `elem` cs
        then Just (NE.nonEmpty rest)
        else do
          rest' <- NE.nonEmpty rest
          new <- go rest'
          pure $ Just $ maybe (c :| []) (c NE.<|) new

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

unfoldArg :: Arg -> NonEmpty String
unfoldArg = \case
  ArgBareDoubleDash -> "--" :| []
  ArgBareDash -> "-" :| []
  ArgDashed l cs -> NE.map renderDashed (unfoldDasheds l cs)
  ArgPlain a -> a :| []

unfoldDasheds :: Bool -> NonEmpty Char -> NonEmpty Dashed
unfoldDasheds b s =
  if b
    then DashedLong s :| []
    else NE.map DashedShort s

data Dashed
  = DashedShort !Char
  | DashedLong !(NonEmpty Char)
  deriving (Show, Eq, Ord, Generic)

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
