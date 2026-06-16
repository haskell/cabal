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
  | -- | @build-type: Make@ is no longer functional, but we must keep parsing
    -- old Cabal files, even if only to throw an error later.
    Make
  | -- | uses user-supplied @Setup.hs@ or @Setup.lhs@ (default)
    Custom
  | Hooks
  deriving (Generic, Show, Read, Eq, Ord, Data)

instance Binary BuildType
instance Structured BuildType
instance NFData BuildType

knownBuildTypes :: [BuildType]
knownBuildTypes = [Simple, Configure, Make, Custom, Hooks]

instance Pretty BuildType where
  pretty = Disp.text . show

instance Parsec BuildType where
  parsec = do
    name <- P.munch1 isAlphaNum
    case name of
      "Simple" -> return Simple
      "Configure" -> return Configure
      "Custom" -> return Custom
      "Make" -> do
        v <- askCabalSpecVersion
        if v < CabalSpecV3_18
          then return Make
          else fail "build-type: 'Make'. This feature requires cabal-version <= 3.18."
      "Hooks" -> do
        v <- askCabalSpecVersion
        if v >= CabalSpecV3_14
          then return Hooks
          else fail "build-type: 'Hooks'. This feature requires cabal-version >= 3.14."
      "Default" -> do
        v <- askCabalSpecVersion
        if v <= CabalSpecV1_18 -- oldest version needing this, based on hackage-tests
          then do
            parsecWarning PWTBuildTypeDefault "build-type: Default is parsed as Custom for legacy reasons. See https://github.com/haskell/cabal/issues/5020"
            return Custom
          else fail ("unknown build-type: '" ++ name ++ "'")
      _ -> fail ("unknown build-type: '" ++ name ++ "'")
