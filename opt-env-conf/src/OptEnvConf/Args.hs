{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module OptEnvConf.Args
  ( Args (..),
    emptyArgs,
    parseArgs,
    consumeArgument,
    consumeArgument',
    consumeArgument'',
    consumeOption,
    consumeOption',
    consumeOption'',
    consumeSwitch,
    consumeSwitch',
    consumeSwitch'',
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
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)
import OptEnvConf.NonDet

newtype Args = Args
  { unArgs :: [Arg]
  }
  deriving (Show, Eq, Generic)

instance Validity Args

emptyArgs :: Args
emptyArgs = Args []

parseArgs :: [String] -> Args
parseArgs args = Args $ map parseArg args

-- This may be accidentally quadratic.
-- We can probably make it faster by having a stack of only args
--
-- The type is a bit strange, but it makes dealing with the state monad easier
consumeArgument :: Args -> (Maybe String, Args)
consumeArgument am =
  let (mS, opts') = go $ unArgs am
   in (mS, am {unArgs = opts'})
  where
    go = \case
      [] -> (Nothing, [])
      (o : rest) -> case o of
        ArgPlain a -> (Just a, rest)
        ArgBareDoubleDash -> goPlain rest
        _ ->
          let (mS, os) = go rest
           in (mS, o : os)
    goPlain = \case
      [] -> (Nothing, [])
      (a : rest) -> (Just (renderArg a), rest)

consumeArgument' :: (Monad m) => Args -> NonDetT m (String, Args)
consumeArgument' am = do
  (s, opts') <- go $ unArgs am
  pure (s, am {unArgs = opts'})
  where
    go :: (Monad m) => [Arg] -> NonDetT m (String, [Arg])
    go = \case
      -- Nothing to consume
      [] -> empty
      -- Every arg could be the argument we consume.
      (o : rest) ->
        pure (renderArg o, rest) <|> do
          (r, others) <- go rest
          pure (r, o : others)

consumeArgument'' :: (Monad m) => NonDetT (StateT Args m) String
consumeArgument'' = do
  as <- gets unArgs
  go [] as
  where
    go :: (Monad m) => [Arg] -> [Arg] -> NonDetT (StateT Args m) String
    go argsBefore = \case
      -- Nothing to consume
      [] -> empty
      -- Every arg could be the argument we consume.
      (o : rest) ->
        -- Consume the arg
        ( do
            -- Delete the o from the arguments because it's been consumed
            put (Args $ reverse argsBefore ++ rest)
            pure (renderArg o)
        )
          -- Or don't consume the arg
          -- ... that's the question
          <|> go (o : argsBefore) rest

-- This may be accidentally cubic.
-- We can probably make this faster by having an actual (Map (Set Dashed) (NonEmpty String)) insetad of just a list that we consume from.
--
-- The type is a bit strange, but it makes dealing with the state monad easier
consumeOption :: [Dashed] -> Args -> (Maybe String, Args)
consumeOption dasheds am =
  let (mS, opts') = go $ unArgs am
   in (mS, am {unArgs = opts'})
  where
    go =
      \case
        [] -> (Nothing, [])
        [a] -> (Nothing, [a])
        (k : v : rest) -> case k of
          ArgDashed isLong cs
            | NE.last (unfoldDasheds isLong cs) `elem` dasheds ->
                (pure (renderArg v), rest)
          _ ->
            let (mS, as) = go (v : rest)
             in (mS, k : as)

consumeOption' :: [Dashed] -> Args -> NonDetT m (String, Args)
consumeOption' = undefined

consumeOption'' :: [Dashed] -> NonDetT (StateT Args m) String
consumeOption'' = undefined

consumeSwitch :: [Dashed] -> Args -> (Maybe (), Args)
consumeSwitch dasheds am =
  let (mS, opts') = go $ unArgs am
   in (mS, am {unArgs = opts'})
  where
    go =
      \case
        [] -> (Nothing, [])
        (o : rest) -> case o of
          ArgDashed isLong cs
            | NE.last (unfoldDasheds isLong cs) `elem` dasheds ->
                (Just (), rest)
          _ ->
            let (mS, os) = go rest
             in (mS, o : os)

consumeSwitch' :: [Dashed] -> Args -> NonDetT m ((), Args)
consumeSwitch' = undefined

consumeSwitch'' :: [Dashed] -> NonDetT (StateT Args m) ()
consumeSwitch'' = undefined

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
