{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Distribution.Client.SetupHooks.Version
  ( HooksVersion(..), hooksVersion )
  where

-- base
import Data.Proxy
  ( Proxy(Proxy) )
import GHC.Generics
  ( Generic )

-- Cabal-syntax
import Distribution.Compat.Binary
  ( Binary )
import Distribution.Types.Version
  ( Version )
import Distribution.Utils.Structured
  ( Structured, MD5, structureHash )

-- Cabal
import Distribution.Simple.SetupHooks.Rule
  ( RuleId, Rule, RuleBinary )
import Distribution.Simple.SetupHooks.Internal
  ( PreConfPackageInputs
  , PreConfPackageOutputs, PostConfPackageInputs
  , PreConfComponentInputs
  , PreConfComponentOutputs
  , PreBuildComponentInputs, PostBuildComponentInputs
  , InstallComponentInputs
  )
import Distribution.Simple.Utils
  ( cabalVersion )
import Distribution.Types.LocalBuildInfo
  ( LocalBuildInfo )

--------------------------------------------------------------------------------

-- | The version of the Hooks API in use.
--
-- Used for handshake before beginning inter-process communication.
data HooksVersion =
  HooksVersion
    { hooksAPIVersion :: !Version
    , cabalABIHash :: !MD5
    , hooksABIHash :: !MD5
    }
  deriving stock ( Eq, Ord, Show, Generic )
  deriving anyclass Binary

-- | The version of the Hooks API in use.
--
-- Used for handshake before beginning inter-process communication.
hooksVersion :: HooksVersion
hooksVersion = HooksVersion
  { hooksAPIVersion = cabalVersion
  , cabalABIHash = structureHash $ Proxy @CabalABI
  , hooksABIHash = structureHash $ Proxy @HooksABI
  }

--------------------------------------------------------------------------------

-- | This datatype keeps track of the parts of the Cabal API which are
-- relevant to its binary interface.
data CabalABI
  = CabalABI
  { cabalLocalBuildInfo :: LocalBuildInfo }
  deriving stock Generic
deriving anyclass instance Structured CabalABI

-- | This datatype keeps track of the parts of the Hooks API which are
-- relevant to its binary interface.
data HooksABI
  = HooksABI
  { confHooks :: ( ( PreConfPackageInputs, PreConfPackageOutputs )
                 , PostConfPackageInputs
                 , ( PreConfComponentInputs, PreConfComponentOutputs ) )
  , buildHooks :: ( PreBuildComponentInputs, ( RuleId, Rule, RuleBinary )
                  , PostBuildComponentInputs )
  , installHooks :: InstallComponentInputs
  }
  deriving stock Generic
deriving anyclass instance Structured HooksABI
