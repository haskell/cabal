{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Types.BuildType
  ( BuildType (..)
  , knownBuildTypes
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion (CabalSpecVersion (..))
import Distribution.Parsec
import Distribution.Pretty

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

-- | The type of build system used by this package.
data BuildType
  = -- | calls @Distribution.Simple.defaultMain@
    Simple
  | -- | calls @Distribution.Simple.defaultMainWithHooks defaultUserHooks@,
    -- which invokes @configure@ to generate additional build
    -- information used by later phases.
    Configure
  | -- | calls @Distribution.Make.defaultMain@
    Make
  | -- | uses user-supplied @Setup.hs@ or @Setup.lhs@ (default)
    Custom
  deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)

instance Binary BuildType
instance Structured BuildType
instance NFData BuildType where rnf = genericRnf

knownBuildTypes :: [BuildType]
knownBuildTypes = [Simple, Configure, Make, Custom]

instance Pretty BuildType where
  pretty = Disp.text . show

instance Parsec BuildType where
  parsec = do
    name <- P.munch1 isAlphaNum
    case name of
      "Simple" -> return Simple
      "Configure" -> return Configure
      "Custom" -> return Custom
      "Make" -> return Make
      "Default" -> do
        v <- askCabalSpecVersion
        if v <= CabalSpecV1_18 -- oldest version needing this, based on hackage-tests
          then do
            parsecWarning PWTBuildTypeDefault "build-type: Default is parsed as Custom for legacy reasons. See https://github.com/haskell/cabal/issues/5020"
            return Custom
          else fail ("unknown build-type: '" ++ name ++ "'")
      _ -> fail ("unknown build-type: '" ++ name ++ "'")
