{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module OptEnvConf.ArgMap
  ( ArgMap (..),
    empty,
    hasUnconsumed,
    Dashed (..),
    parse,
    consumeArg,
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
  = DashedShort !(NonEmpty Char)
  | DashedLong !(NonEmpty Char)
  deriving (Show, Eq, Ord, Generic)

instance Validity Dashed where
  validate d =
    mconcat
      [ genericValidate d,
        case d of
          DashedLong _ -> valid
          DashedShort (c :| _) -> declare "does not start with a dash" $ c /= '-'
      ]

parse :: [String] -> ArgMap
parse = go
  where
    go :: [String] -> ArgMap
    go = \case
      [] -> empty
      ("--" : leftovers) -> empty {argMapLeftovers = leftovers}
      ("-" : rest) ->
        let am = go rest
         in am {argMapArgs = "-" : argMapArgs am}
      (('-' : opt) : rest) ->
        let asSwitch =
              let am = go rest
                  d = parseDashed opt
               in am {argMapSwitches = d : argMapSwitches am}
         in case rest of
              [] -> asSwitch
              (next : others)
                | isFlag next -> asSwitch
                | otherwise ->
                    let am = go others
                        d = parseDashed opt
                     in am {argMapOptions = M.insertWith (<>) d (next :| []) (argMapOptions am)}
      (a : rest) ->
        let am = go rest
         in am {argMapArgs = a : argMapArgs am}

    parseDashed :: String -> Dashed
    parseDashed = \case
      '-' : rest -> DashedLong $ NE.fromList rest
      rest -> DashedShort $ NE.fromList rest

    isFlag :: String -> Bool
    isFlag = \case
      "--" -> False
      "-" -> False
      ('-' : '-' : _) -> True
      ('-' : _) -> True
      _ -> False

-- The type is a bit strange, but it makes dealing with the state monad easier
consumeArg :: ArgMap -> (Maybe String, ArgMap)
consumeArg am = case argMapArgs am of
  [] -> (Nothing, am)
  (a : rest) -> (Just a, am {argMapArgs = rest})
