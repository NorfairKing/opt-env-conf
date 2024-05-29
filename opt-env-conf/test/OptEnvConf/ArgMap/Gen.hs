module OptEnvConf.ArgMap.Gen where

import Data.GenValidity
import Data.GenValidity.Containers ()
import OptEnvConf.ArgMap

instance GenValid Dashed

instance GenValid ArgMap
