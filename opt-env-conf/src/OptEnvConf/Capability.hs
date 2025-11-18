{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OptEnvConf.Capability
  ( Capabilities (..),
    Capability (..),
    allCapabilities,
    enableCapability,
    disableCapability,
    missingCapabilities,

    -- * Predefined capabilities
    readSecretCapability,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Text (Text)
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Text ()
import GHC.Generics (Generic)

-- Set of disabled capabilities
newtype Capabilities = Capabilities {unCapabilities :: Set Capability}
  deriving (Show, Generic)

instance Validity Capabilities

allCapabilities :: Capabilities
allCapabilities = Capabilities {unCapabilities = Set.empty}

enableCapability :: Capability -> Capabilities -> Capabilities
enableCapability cap (Capabilities caps) =
  Capabilities (Set.delete cap caps)

disableCapability :: Capability -> Capabilities -> Capabilities
disableCapability cap (Capabilities caps) =
  Capabilities (Set.insert cap caps)

missingCapabilities :: Capabilities -> Set Capability -> Maybe (NonEmpty Capability)
missingCapabilities (Capabilities caps) requiredCapabilities =
  NE.nonEmpty (Set.toList (Set.intersection requiredCapabilities caps))

newtype Capability = Capability {unCapability :: Text}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, IsString)

instance Validity Capability

-- | The annotation for any setting reading secrets.
--
-- We add these so that we can disable them in settings checks, to avoid
-- failing settings checks when secrets are read at runtime instead of
-- build-time.
readSecretCapability :: String
readSecretCapability = "read-secret"
