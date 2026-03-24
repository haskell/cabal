-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.GHC.ImplInfo
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains the data structure describing invocation
-- details for a GHC or GHC-derived compiler, such as supported flags
-- and workarounds for bugs.
module Distribution.Simple.GHC.ImplInfo
  ( GhcImplInfo (..)
  , getImplInfo
  , ghcVersionImplInfo
  , ghcjsVersionImplInfo
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Simple.Compiler
import Distribution.Version

-- |
--      Information about features and quirks of a GHC-based implementation.
--
--      Compiler flavors based on GHC behave similarly enough that some of
--      the support code for them is shared. Every implementation has its
--      own peculiarities, that may or may not be a direct result of the
--      underlying GHC version. This record keeps track of these differences.
--
--      All shared code (i.e. everything not in the Distribution.Simple.FLAVOR
--      module) should use implementation info rather than version numbers
--      to test for supported features.
data GhcImplInfo = GhcImplInfo
  { supportsGHC2021 :: Bool
  -- ^ -XGHC2021 flag
  , supportsGHC2024 :: Bool
  -- ^ -XGHC2024 flag
  , flagProfLate :: Bool
  -- ^ fprof-late flag
  , flagHie :: Bool
  -- ^ -hiedir flag supported
  , flagWarnMissingHomeModules :: Bool
  -- ^ -Wmissing-home-modules is supported
  , unitIdForExes :: Bool
  -- ^ Pass -this-unit-id flag when building executables
  }

getImplInfo :: Compiler -> GhcImplInfo
getImplInfo comp =
  case compilerFlavor comp of
    GHC -> ghcVersionImplInfo (compilerVersion comp)
    GHCJS -> case compilerCompatVersion GHC comp of
      Just ghcVer -> ghcjsVersionImplInfo (compilerVersion comp) ghcVer
      _ ->
        error
          ( "Distribution.Simple.GHC.Props.getImplProps: "
              ++ "could not find GHC version for GHCJS compiler"
          )
    x ->
      error
        ( "Distribution.Simple.GHC.Props.getImplProps only works"
            ++ "for GHC-like compilers (GHC, GHCJS)"
            ++ ", but found "
            ++ show x
        )

ghcVersionImplInfo :: Version -> GhcImplInfo
ghcVersionImplInfo ver =
  GhcImplInfo
    { supportsGHC2021 = v >= [9, 1]
    , supportsGHC2024 = v >= [9, 9]
    , flagProfLate = v >= [9, 4]
    , flagHie = v >= [8, 8]
    , flagWarnMissingHomeModules = v >= [8, 2]
    , unitIdForExes = v >= [9, 2]
    }
  where
    v = versionNumbers ver

ghcjsVersionImplInfo
  :: Version
  -- ^ The GHCJS version
  -> Version
  -- ^ The GHC version
  -> GhcImplInfo
ghcjsVersionImplInfo _ghcjsver ghcver =
  GhcImplInfo
    { supportsGHC2021 = True
    , supportsGHC2024 = ghcv >= [9, 9]
    , flagProfLate = True
    , flagHie = ghcv >= [8, 8]
    , flagWarnMissingHomeModules = ghcv >= [8, 2]
    , unitIdForExes = ghcv >= [9, 2]
    }
  where
    ghcv = versionNumbers ghcver
