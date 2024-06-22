{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)

newtype Args = Args
  { unArgs :: [Arg]
  }
  deriving (Show, Eq, Generic)

instance Validity Args

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
        -- TODO put this option at the back so it's considered last
        (renderArg o1, o2 : rest)
          : (renderArg o2, o1 : rest)
          : map (second ((o1 :) . (o2 :))) (go rest)
      -- A plain argument could definitely be an argument.
      (ArgPlain a : rest) -> [(a, rest)]

-- This may be accidentally cubic.
-- We can probably make this faster by having an actual (Map (Set Dashed) (NonEmpty String)) insetad of just a list that we consume from.
--
-- The type is a bit strange, but it makes dealing with the state monad easier
consumeOption :: [Dashed] -> Args -> [(String, Args)]
consumeOption dasheds am = do
  (mS, opts') <- go $ unArgs am
  pure (mS, am {unArgs = opts'})
  where
    go :: [Arg] -> [(String, [Arg])]
    go = \case
      [] -> []
      [_] -> []
      (k : v : rest) -> case k of
        ArgDashed isLong cs
          | NE.last (unfoldDasheds isLong cs) `elem` dasheds ->
              [(renderArg v, rest)]
        _ -> do
          (mS, as) <- go (v : rest)
          pure (mS, k : as)

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
