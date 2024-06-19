{-# OPTIONS_GHC -Wno-orphans #-}

module OptEnvConf.Gen where

import Data.GenValidity
import Data.GenValidity.Containers ()
import OptEnvConf.ArgMap (Dashed)

instance GenValid Dashed
