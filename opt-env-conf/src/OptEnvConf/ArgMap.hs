{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module OptEnvConf.ArgMap
  ( ArgMap (..),
    empty,
    hasUnconsumed,
    Dashed (..),
    parse,
    consumeArg,
    parseSingleArg,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)

data ArgMap = ArgMap
  { argMapArgs :: ![String],
    argMapSwitches :: ![Dashed],
    argMapOptions :: !(Map Dashed (NonEmpty String)),
    argMapLeftovers :: ![String]
  }
  deriving (Show, Eq, Generic)

instance Validity ArgMap

empty :: ArgMap
empty =
  ArgMap
    { argMapArgs = [],
      argMapSwitches = [],
      argMapOptions = M.empty,
      argMapLeftovers = []
    }

hasUnconsumed :: ArgMap -> Bool
hasUnconsumed am =
  not (null (argMapArgs am))
    || not (null (argMapSwitches am))
    || not (null (argMapOptions am))

data Dashed
  = DashedShort !Char
  | DashedLong !(NonEmpty Char)
  deriving (Show, Eq, Ord, Generic)

instance Validity Dashed

parse :: [String] -> ArgMap
parse = go
  where
    go :: [String] -> ArgMap
    go = \case
      [] -> empty
      (a : rest) ->
        let am = go rest
         in case parseSingleArg a of
              ArgBareDoubleDash -> empty {argMapLeftovers = rest}
              ArgBareDash -> am {argMapArgs = "-" : argMapArgs am}
              ArgDashed isLong opt ->
                let ds = parseDasheds isLong opt
                    asSwitch = am {argMapSwitches = ds <> argMapSwitches am}
                 in case rest of
                      [] -> asSwitch
                      (next : others)
                        | isDashed (parseSingleArg next) -> asSwitch
                        | otherwise ->
                            let am' = go others
                                m = M.fromList $ map (\d -> (d, next :| [])) ds
                             in am' {argMapOptions = M.unionWith (<>) m (argMapOptions am)}
              ArgPlain plainArg -> am {argMapArgs = plainArg : argMapArgs am}

    parseDasheds :: Bool -> NonEmpty Char -> [Dashed]
    parseDasheds b s =
      if b
        then [DashedLong s]
        else map DashedShort (NE.toList s)

    isDashed :: Arg -> Bool
    isDashed = \case
      ArgDashed _ _ -> True
      _ -> False

-- The type is a bit strange, but it makes dealing with the state monad easier
consumeArg :: ArgMap -> (Maybe String, ArgMap)
consumeArg am = case argMapArgs am of
  [] -> (Nothing, am)
  (a : rest) -> (Just a, am {argMapArgs = rest})

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
