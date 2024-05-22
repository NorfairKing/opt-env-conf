{-# LANGUAGE DeriveGeneric #-}

module OptEnvConf.EnvMap
  ( EnvMap (..),
    empty,
    parse,
    lookup,
    insert,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)
import Prelude hiding (lookup)

-- | Abstraction for storing and looking up environment variables
--
-- This can be faster than [(String, String)] and also cannot represent
-- duplicate keys.
newtype EnvMap = EnvMap {unEnvMap :: Map String String}
  deriving (Show, Generic)

instance Validity EnvMap

empty :: EnvMap
empty = EnvMap {unEnvMap = M.empty}

parse :: [(String, String)] -> EnvMap
parse = EnvMap . M.fromList -- TODO fail if there are duplicate keys.

lookup :: String -> EnvMap -> Maybe String
lookup v (EnvMap m) = M.lookup v m

insert :: String -> String -> EnvMap -> EnvMap
insert k v (EnvMap m) = EnvMap (M.insert k v m)
