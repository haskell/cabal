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
  { supportsHaskell2010 :: Bool
  -- ^ -XHaskell2010 and -XHaskell98 flags
  , supportsGHC2021 :: Bool
  -- ^ -XGHC2021 flag
  , supportsGHC2024 :: Bool
  -- ^ -XGHC2024 flag
  , reportsNoExt :: Bool
  -- ^ --supported-languages gives Ext and NoExt
  , alwaysNondecIndent :: Bool
  -- ^ NondecreasingIndentation is always on
  , flagGhciScript :: Bool
  -- ^ -ghci-script flag supported
  , flagProfAuto :: Bool
  -- ^ new style -fprof-auto* flags
  , flagProfLate :: Bool
  -- ^ fprof-late flag
  , flagPackageConf :: Bool
  -- ^ use package-conf instead of package-db
  , flagDebugInfo :: Bool
  -- ^ -g flag supported
  , flagHie :: Bool
  -- ^ -hiedir flag supported
  , supportsDebugLevels :: Bool
  -- ^ supports numeric @-g@ levels
  , supportsPkgEnvFiles :: Bool
  -- ^ picks up @.ghc.environment@ files
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
    { supportsHaskell2010 = v >= [7]
    , supportsGHC2021 = v >= [9, 1]
    , supportsGHC2024 = v >= [9, 9]
    , reportsNoExt = v >= [7]
    , alwaysNondecIndent = v < [7, 1]
    , flagGhciScript = v >= [7, 2]
    , flagProfAuto = v >= [7, 4]
    , flagProfLate = v >= [9, 4]
    , flagPackageConf = v < [7, 5]
    , flagDebugInfo = v >= [7, 10]
    , flagHie = v >= [8, 8]
    , supportsDebugLevels = v >= [8, 0]
    , supportsPkgEnvFiles = v >= [8, 0, 1, 20160901] -- broken in 8.0.1, fixed in 8.0.2
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
    { supportsHaskell2010 = True
    , supportsGHC2021 = True
    , supportsGHC2024 = ghcv >= [9, 9]
    , reportsNoExt = True
    , alwaysNondecIndent = False
    , flagGhciScript = True
    , flagProfAuto = True
    , flagProfLate = True
    , flagPackageConf = False
    , flagDebugInfo = False
    , flagHie = ghcv >= [8, 8]
    , supportsDebugLevels = ghcv >= [8, 0]
    , supportsPkgEnvFiles = ghcv >= [8, 0, 2] -- TODO: check this works in ghcjs
    , flagWarnMissingHomeModules = ghcv >= [8, 2]
    , unitIdForExes = ghcv >= [9, 2]
    }
  where
    ghcv = versionNumbers ghcver
