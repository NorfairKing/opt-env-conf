{-# OPTIONS_GHC -Wno-orphans #-}

module OptEnvConf.EnvMap.Gen where

import Data.GenValidity
import Data.GenValidity.Containers ()
import OptEnvConf.EnvMap

instance GenValid EnvMap
