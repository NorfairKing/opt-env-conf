{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module OptEnvConf.ArgMap
  ( ArgMap (..),
    emptyArgMap,
    Dashed (..),
    parseArgMap,
  )
where

import Control.Applicative
import Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
-- import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as JSON
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)
import System.Environment (getArgs, getEnvironment)
import System.Exit

data ArgMap = ArgMap
  { argMapArgs :: ![String],
    argMapSwitches :: ![Dashed],
    argMapOptions :: !(Map Dashed (NonEmpty String)),
    argMapLeftovers :: ![String]
  }
  deriving (Show, Eq, Generic)

instance Validity ArgMap

emptyArgMap :: ArgMap
emptyArgMap =
  ArgMap
    { argMapArgs = [],
      argMapSwitches = [],
      argMapOptions = M.empty,
      argMapLeftovers = []
    }

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

parseArgMap :: [String] -> ArgMap
parseArgMap = go
  where
    go :: [String] -> ArgMap
    go = \case
      [] -> emptyArgMap
      ("--" : leftovers) -> emptyArgMap {argMapLeftovers = leftovers}
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
