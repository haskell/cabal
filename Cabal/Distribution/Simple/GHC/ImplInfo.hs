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

import Distribution.Simple.Compiler
  ( Compiler(..), CompilerFlavor(..)
  , compilerFlavor, compilerVersion, compilerCompatVersion )
import Distribution.Version ( Version(..) )

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
  { hasCcOdirBug         :: Bool -- ^ bug in -odir handling for C compilations.
  , flagInfoLanguages    :: Bool -- ^ --info and --supported-languages flags
  , fakeRecordPuns       :: Bool -- ^ use -XRecordPuns for NamedFieldPuns
  , flagStubdir          :: Bool -- ^ -stubdir flag supported
  , flagOutputDir        :: Bool -- ^ -outputdir flag supported
  , noExtInSplitSuffix   :: Bool -- ^ split-obj suffix does not contain p_o ext
  , flagFfiIncludes      :: Bool -- ^ -#include on command line for FFI includes
  , flagBuildingCabalPkg :: Bool -- ^ -fbuilding-cabal-package flag supported
  , flagPackageId        :: Bool -- ^ -package-id / -package flags supported
  , separateGccMingw     :: Bool -- ^ mingw and gcc are in separate directories
  , supportsHaskell2010  :: Bool -- ^ -XHaskell2010 and -XHaskell98 flags
  , reportsNoExt         :: Bool -- ^ --supported-languages gives Ext and NoExt
  , alwaysNondecIndent   :: Bool -- ^ NondecreasingIndentation is always on
  , flagGhciScript       :: Bool -- ^ -ghci-script flag supported
  , flagProfAuto         :: Bool -- ^ new style -fprof-auto* flags
  , flagPackageConf      :: Bool -- ^ use package-conf instead of package-db
  , flagDebugInfo        :: Bool -- ^ -g flag supported
  , supportsMultInst     :: Bool -- ^ ghc-pkg can register multiple instances of
                                 --   same version of package
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
  { hasCcOdirBug         = v <  [6,4,1]
  , flagInfoLanguages    = v >= [6,7]
  , fakeRecordPuns       = v >= [6,8] && v < [6,10]
  , flagStubdir          = v >= [6,8]
  , flagOutputDir        = v >= [6,10]
  , noExtInSplitSuffix   = v <  [6,11]
  , flagFfiIncludes      = v <  [6,11]
  , flagBuildingCabalPkg = v >= [6,11]
  , flagPackageId        = v >  [6,11]
  , separateGccMingw     = v <  [6,12]
  , supportsHaskell2010  = v >= [7]
  , reportsNoExt         = v >= [7]
  , alwaysNondecIndent   = v <  [7,1]
  , flagGhciScript       = v >= [7,2]
  , flagProfAuto         = v >= [7,4]
  , flagPackageConf      = v <  [7,5]
  , flagDebugInfo        = v >= [7,10]
  , supportsMultInst     = v >= [7,11]
  }

ghcjsVersionImplInfo :: Version -> Version -> GhcImplInfo
ghcjsVersionImplInfo _ghcjsVer _ghcVer = GhcImplInfo
  { hasCcOdirBug         = False
  , flagInfoLanguages    = True
  , fakeRecordPuns       = False
  , flagStubdir          = True
  , flagOutputDir        = True
  , noExtInSplitSuffix   = False
  , flagFfiIncludes      = False
  , flagBuildingCabalPkg = True
  , flagPackageId        = True
  , separateGccMingw     = False
  , supportsHaskell2010  = True
  , reportsNoExt         = True
  , alwaysNondecIndent   = False
  , flagGhciScript       = True
  , flagProfAuto         = True
  , flagPackageConf      = False
  , flagDebugInfo        = False
  , supportsMultInst     = False
  }

lhcVersionImplInfo :: Version -> GhcImplInfo
lhcVersionImplInfo = ghcVersionImplInfo
