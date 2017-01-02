{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.GenericPackageDescription (
    GenericPackageDescription(..),
    Flag(..),
    emptyFlag,
    FlagName,
    mkFlagName,
    unFlagName,
    FlagAssignment,
    ConfVar(..),
) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Utils.ShortText

import Distribution.Types.PackageDescription

import Distribution.Types.Dependency
import Distribution.Types.Library
import Distribution.Types.ForeignLib
import Distribution.Types.Executable
import Distribution.Types.TestSuite
import Distribution.Types.Benchmark
import Distribution.Types.UnqualComponentName
import Distribution.Types.CondTree

import Distribution.Package
import Distribution.Version
import Distribution.Compiler
import Distribution.System

-- ---------------------------------------------------------------------------
-- The GenericPackageDescription type

data GenericPackageDescription =
    GenericPackageDescription {
        packageDescription :: PackageDescription,
        genPackageFlags    :: [Flag],
        condLibrary        :: Maybe (CondTree ConfVar [Dependency] Library),
        condSubLibraries   :: [(UnqualComponentName, CondTree ConfVar [Dependency] Library)],
        condForeignLibs    :: [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)],
        condExecutables    :: [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)],
        condTestSuites     :: [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)],
        condBenchmarks     :: [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)]
      }
    deriving (Show, Eq, Typeable, Data, Generic)

instance Package GenericPackageDescription where
  packageId = packageId . packageDescription

instance Binary GenericPackageDescription

-- | A flag can represent a feature to be included, or a way of linking
--   a target against its dependencies, or in fact whatever you can think of.
data Flag = MkFlag
    { flagName        :: FlagName
    , flagDescription :: String
    , flagDefault     :: Bool
    , flagManual      :: Bool
    }
    deriving (Show, Eq, Typeable, Data, Generic)

instance Binary Flag

-- | A 'Flag' initialized with default parameters.
emptyFlag :: FlagName -> Flag
emptyFlag name = MkFlag
    { flagName        = name
    , flagDescription = ""
    , flagDefault     = True
    , flagManual      = False
    }

-- | A 'FlagName' is the name of a user-defined configuration flag
--
-- Use 'mkFlagName' and 'unFlagName' to convert from/to a 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0
newtype FlagName = FlagName ShortText
    deriving (Eq, Generic, Ord, Show, Read, Typeable, Data)

-- | Construct a 'FlagName' from a 'String'
--
-- 'mkFlagName' is the inverse to 'unFlagName'
--
-- Note: No validations are performed to ensure that the resulting
-- 'FlagName' is valid
--
-- @since 2.0
mkFlagName :: String -> FlagName
mkFlagName = FlagName . toShortText

-- | Convert 'FlagName' to 'String'
--
-- @since 2.0
unFlagName :: FlagName -> String
unFlagName (FlagName s) = fromShortText s

instance Binary FlagName

-- | A 'FlagAssignment' is a total or partial mapping of 'FlagName's to
-- 'Bool' flag values. It represents the flags chosen by the user or
-- discovered during configuration. For example @--flags=foo --flags=-bar@
-- becomes @[("foo", True), ("bar", False)]@
--
type FlagAssignment = [(FlagName, Bool)]

-- | A @ConfVar@ represents the variable type used.
data ConfVar = OS OS
             | Arch Arch
             | Flag FlagName
             | Impl CompilerFlavor VersionRange
    deriving (Eq, Show, Typeable, Data, Generic)

instance Binary ConfVar
