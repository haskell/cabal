-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Reporting
-- Copyright   :  (c) David Waern 2008
-- License     :  BSD-like
--
-- Maintainer  :  david.waern@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Report data structure
--
-----------------------------------------------------------------------------
module Hackage.Reporting (
    BuildReport(..),
    ConfigurePhase(..),
    BuildPhase(..),
    InstallPhase(..),
    Outcome(..),
    writeBuildReport,
    makeSuccessReport,
  ) where

import Hackage.Types
         ( ConfiguredPackage(..) )

import Distribution.Package
         ( PackageIdentifier, Package(packageId) )
import Distribution.PackageDescription
         ( FlagAssignment )
import Distribution.System
         ( OS, Arch )
import Distribution.Compiler
         ( CompilerId )

import System.FilePath
         ( takeDirectory )
import System.Directory
         ( createDirectoryIfMissing )


data BuildReport = BuildReport {
    -- | The package this build report is about
    buildPackage           :: PackageIdentifier,

    -- | The OS and Arch the package was built on
    buildOS                :: OS,
    buildArch              :: Arch,

    -- | The Haskell compiler (and hopefully version) used
    buildCompiler          :: CompilerId,

    -- | Configure outcome, did configure work ok?
    buildOutcomeConfigure  :: Outcome ConfigurePhase
  }
  deriving (Show, Read)


data ConfigurePhase = ConfigurePhase {
    -- | Which configurations flags we used
    buildFlagAssignment  :: FlagAssignment,

    -- | Which dependent packages we were using exactly
    buildResolvedDeps    :: [PackageIdentifier],

    -- | Which build tools we were using (with versions)
--    buildResolvedTools   :: [PackageIdentifier],

    -- | Build outcome, did the build phase work ok?
    buildOutcomeBuild    :: Outcome BuildPhase

    -- | Build outcome, did building the docs work?
--    buildOutcomeDocs     :: Outcome DocsPhase
  }
  deriving (Show, Read)


data BuildPhase = BuildPhase {
    -- | Build outcome, did installing work ok?
    buildOutcomeInstall  :: Outcome InstallPhase
  }
  deriving (Show, Read)

--data DocsPhase = DocsPhase deriving (Show, Read)

data InstallPhase = InstallPhase deriving (Show, Read)

data Outcome a = OutcomeOk a | OutcomeFailed | OutcomeNotTried
  deriving (Show, Read)


writeBuildReport :: FilePath -> BuildReport -> IO ()
writeBuildReport file report = do
  createDirectoryIfMissing True (takeDirectory file)
  writeFile file $ show report


makeSuccessReport :: OS -> Arch -> CompilerId
                  -> ConfiguredPackage -> BuildReport
makeSuccessReport os arch comp (ConfiguredPackage pkg flags deps) =
  BuildReport {
    buildPackage             = packageId pkg,
    buildOS                  = os,
    buildArch                = arch,
    buildCompiler            = comp,
    buildOutcomeConfigure    = OutcomeOk ConfigurePhase { 
      buildFlagAssignment    = flags,
      buildResolvedDeps      = deps,
      buildOutcomeBuild      = OutcomeOk BuildPhase {
        buildOutcomeInstall  = OutcomeOk InstallPhase
      }
    }
  }

--makeFailureReport :: OS -> Arch -> CompilerId
--                  -> ConfiguredPackage -> BuildReport
