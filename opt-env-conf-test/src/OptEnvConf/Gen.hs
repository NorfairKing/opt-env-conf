{-# OPTIONS_GHC -Wno-orphans #-}

module OptEnvConf.Gen where

import Data.GenValidity
import Data.GenValidity.Containers
import OptEnvConf.ArgMap (Dashed, Possible)

instance GenValid Dashed

instance GenValid Possible
