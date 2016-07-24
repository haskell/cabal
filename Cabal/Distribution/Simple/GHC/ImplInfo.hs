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

module Distribution.Simple.GHC.ImplInfo (
        GhcImplInfo(..), getImplInfo,
        ghcVersionImplInfo, ghcjsVersionImplInfo, lhcVersionImplInfo
        ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Simple.Compiler
import Distribution.Version

{- |
     Information about features and quirks of a GHC-based implementation.

     Compiler flavors based on GHC behave similarly enough that some of
     the support code for them is shared. Every implementation has its
     own peculiarities, that may or may not be a direct result of the
     underlying GHC version. This record keeps track of these differences.

     All shared code (i.e. everything not in the Distribution.Simple.FLAVOR
     module) should use implementation info rather than version numbers
     to test for supported features.
-}

data GhcImplInfo = GhcImplInfo
  { supportsHaskell2010  :: Bool -- ^ -XHaskell2010 and -XHaskell98 flags
  , reportsNoExt         :: Bool -- ^ --supported-languages gives Ext and NoExt
  , alwaysNondecIndent   :: Bool -- ^ NondecreasingIndentation is always on
  , flagGhciScript       :: Bool -- ^ -ghci-script flag supported
  , flagProfAuto         :: Bool -- ^ new style -fprof-auto* flags
  , flagPackageConf      :: Bool -- ^ use package-conf instead of package-db
  , flagDebugInfo        :: Bool -- ^ -g flag supported
  }

getImplInfo :: Compiler -> GhcImplInfo
getImplInfo comp =
  case compilerFlavor comp of
    GHC   -> ghcVersionImplInfo (compilerVersion comp)
    LHC   -> lhcVersionImplInfo (compilerVersion comp)
    GHCJS -> case compilerCompatVersion GHC comp of
              Just ghcVer -> ghcjsVersionImplInfo (compilerVersion comp) ghcVer
              _  -> error ("Distribution.Simple.GHC.Props.getImplProps: " ++
                           "could not find GHC version for GHCJS compiler")
    x     -> error ("Distribution.Simple.GHC.Props.getImplProps only works" ++
                    "for GHC-like compilers (GHC, GHCJS, LHC)" ++
                    ", but found " ++ show x)

ghcVersionImplInfo :: Version -> GhcImplInfo
ghcVersionImplInfo (Version v _) = GhcImplInfo
  { supportsHaskell2010  = v >= [7]
  , reportsNoExt         = v >= [7]
  , alwaysNondecIndent   = v <  [7,1]
  , flagGhciScript       = v >= [7,2]
  , flagProfAuto         = v >= [7,4]
  , flagPackageConf      = v <  [7,5]
  , flagDebugInfo        = v >= [7,10]
  }

ghcjsVersionImplInfo :: Version -> Version -> GhcImplInfo
ghcjsVersionImplInfo _ghcjsVer _ghcVer = GhcImplInfo
  { supportsHaskell2010  = True
  , reportsNoExt         = True
  , alwaysNondecIndent   = False
  , flagGhciScript       = True
  , flagProfAuto         = True
  , flagPackageConf      = False
  , flagDebugInfo        = False
  }

lhcVersionImplInfo :: Version -> GhcImplInfo
lhcVersionImplInfo = ghcVersionImplInfo
