{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module OptEnvConf.ArgMap
  ( ArgMap (..),
    empty,
    Dashed (..),
    renderDashed,
    prefixDashed,
    parse,
    consumeArgument,
    consumeOption,
    consumeSwitch,
    Opt (..),
    parseSingleArg,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)

newtype ArgMap = ArgMap
  { argMapOpts :: [Opt]
  }
  deriving (Show, Eq, Generic)

instance Validity ArgMap

empty :: ArgMap
empty = ArgMap []

data Dashed
  = DashedShort !Char
  | DashedLong !(NonEmpty Char)
  deriving (Show, Eq, Ord, Generic)

instance Validity Dashed

renderDashed :: Dashed -> String
renderDashed = \case
  DashedShort c -> ['-', c]
  DashedLong cs -> '-' : '-' : NE.toList cs

prefixDashed :: String -> Dashed -> Dashed
prefixDashed p = \case
  DashedLong l -> DashedLong $ p `NE.prependList` l
  DashedShort c -> DashedShort c

parse :: [String] -> ArgMap
parse args = ArgMap $ parseOpts args

-- This may be accidentally quadratic.
-- We can probably make it faster by having a stack of only args
--
-- The type is a bit strange, but it makes dealing with the state monad easier
consumeArgument :: ArgMap -> (Maybe String, ArgMap)
consumeArgument am =
  let (mS, opts') = go $ argMapOpts am
   in (mS, am {argMapOpts = opts'})
  where
    go =
      \case
        [] -> (Nothing, [])
        (o : rest) -> case o of
          OptArg v -> (Just v, rest)
          _ ->
            let (mS, os) = go rest
             in (mS, o : os)

-- This may be accidentally cubic.
-- We can probably make this faster by having an actual (Map (Set Dashed) (NonEmpty String)) insetad of just a list that we consume from.
--
-- The type is a bit strange, but it makes dealing with the state monad easier
consumeOption :: [Dashed] -> ArgMap -> (Maybe String, ArgMap)
consumeOption dasheds am =
  let (mS, opts') = go $ argMapOpts am
   in (mS, am {argMapOpts = opts'})
  where
    go =
      \case
        [] -> (Nothing, [])
        (o : rest) -> case o of
          OptOption k v | k `elem` dasheds -> (Just v, rest)
          _ ->
            let (mS, os) = go rest
             in (mS, o : os)

consumeSwitch :: [Dashed] -> ArgMap -> (Maybe (), ArgMap)
consumeSwitch dasheds am =
  let (mS, opts') = go $ argMapOpts am
   in (mS, am {argMapOpts = opts'})
  where
    go =
      \case
        [] -> (Nothing, [])
        (o : rest) -> case o of
          OptSwitch k | k `elem` dasheds -> (Just (), rest)
          _ ->
            let (mS, os) = go rest
             in (mS, o : os)

data Opt
  = OptArg String
  | OptSwitch !Dashed
  | OptOption !Dashed !String
  deriving (Show, Eq, Generic)

instance Validity Opt

parseOpts :: [String] -> [Opt]
parseOpts = go
  where
    go = \case
      [] -> []
      (s : rest) ->
        case parseSingleArg s of
          ArgBareDoubleDash -> map OptArg rest -- All further args are arguments, not options
          ArgBareDash -> OptArg "-" : go rest
          ArgPlain a -> OptArg a : go rest
          ArgDashed isLong key ->
            let ds = parseDasheds isLong key
                asSwitches = map OptSwitch (NE.toList ds) ++ go rest
             in case NE.nonEmpty rest of
                  Nothing -> asSwitches
                  Just (a :| others) ->
                    let asOption v =
                          let ss = NE.init ds
                              o = NE.last ds
                           in (map OptSwitch ss ++ [OptOption o v]) ++ go others
                     in case parseSingleArg a of
                          ArgBareDoubleDash -> asSwitches
                          ArgDashed _ _ -> asSwitches
                          ArgPlain val -> asOption val
                          ArgBareDash -> asOption "-"

    parseDasheds :: Bool -> NonEmpty Char -> NonEmpty Dashed
    parseDasheds b s =
      if b
        then DashedLong s :| []
        else NE.map DashedShort s

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

parseSingleArg :: String -> Arg
parseSingleArg = \case
  '-' : '-' : rest -> case NE.nonEmpty rest of
    Nothing -> ArgBareDoubleDash
    Just ne -> ArgDashed True ne
  '-' : rest -> case NE.nonEmpty rest of
    Nothing -> ArgBareDash
    Just ne -> ArgDashed False ne
  s -> ArgPlain s
