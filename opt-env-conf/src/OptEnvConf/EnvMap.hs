{-# LANGUAGE DeriveGeneric #-}

module OptEnvConf.EnvMap
  ( EnvMap (..),
    empty,
    parse,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)

newtype EnvMap = EnvMap {unEnvMap :: Map String String}
  deriving (Show, Eq, Generic)

instance Validity EnvMap

empty :: EnvMap
empty = EnvMap {unEnvMap = M.empty}

parse :: [(String, String)] -> EnvMap
parse = EnvMap . M.fromList -- TODO fail if there are duplicate keys.
