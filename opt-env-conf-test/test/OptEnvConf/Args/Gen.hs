{-# OPTIONS_GHC -Wno-orphans #-}

module OptEnvConf.Args.Gen where

import Data.GenValidity
import Data.GenValidity.Containers ()
import OptEnvConf.Args

instance GenValid Dashed

instance GenValid Opt

instance GenValid Args
