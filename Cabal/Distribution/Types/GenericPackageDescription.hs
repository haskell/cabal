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
    showFlagValue,
    dispFlagAssignment,
    parseFlagAssignment,
    ConfVar(..),
) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Utils.ShortText
import Distribution.Utils.Generic (lowercase)
import qualified Text.PrettyPrint as Disp
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.ReadP ((+++))

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

-- | 'mkFlagName'
--
-- @since 2.0
instance IsString FlagName where
    fromString = mkFlagName

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

-- | String representation of a flag-value pair.
showFlagValue :: (FlagName, Bool) -> String
showFlagValue (f, True)   = '+' : unFlagName f
showFlagValue (f, False)  = '-' : unFlagName f

-- | Pretty-prints a flag assignment.
dispFlagAssignment :: FlagAssignment -> Disp.Doc
dispFlagAssignment = Disp.hsep . map (Disp.text . showFlagValue)

-- | Parses a flag assignment.
parseFlagAssignment :: Parse.ReadP r FlagAssignment
parseFlagAssignment = Parse.sepBy1 parseFlagValue Parse.skipSpaces1
  where
    parseFlagValue =
          (do Parse.optional (Parse.char '+')
              f <- parseFlagName
              return (f, True))
      +++ (do _ <- Parse.char '-'
              f <- parseFlagName
              return (f, False))
    parseFlagName = liftM (mkFlagName . lowercase) ident

    ident :: Parse.ReadP r String
    ident = Parse.munch1 identChar >>= \s -> check s >> return s
      where
        identChar c   = isAlphaNum c || c == '_' || c == '-'
        check ('-':_) = Parse.pfail
        check _       = return ()

-- | A @ConfVar@ represents the variable type used.
data ConfVar = OS OS
             | Arch Arch
             | Flag FlagName
             | Impl CompilerFlavor VersionRange
    deriving (Eq, Show, Typeable, Data, Generic)

instance Binary ConfVar
