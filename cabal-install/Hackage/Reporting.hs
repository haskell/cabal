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
module Hackage.Reporting where


import Distribution.Package
import Distribution.System
import Distribution.Compiler
import Distribution.Version
import System.FilePath


data BuildReport = BuildReport {
  -- | The package this build report is about
  buildPackage           :: PackageIdentifier,

  -- | The OS and arch the package was built on
  buildPlatform          :: (OS, String),

  -- | The Haskell compiler (and maybe version) used
  buildCompiler          :: (CompilerFlavor, Maybe Version),

  -- | Configure outcome, did configure work ok?
  buildOutcomeConfigure  :: Outcome ConfigurePhase
}
  deriving (Show, Read)


data ConfigurePhase = ConfigurePhase {
  -- | Which dependent packages we're using exactly
  buildResolvedDeps    :: [PackageIdentifier],

  -- | Which build tools where are using (with versions)
  buildResolvedTools   :: [Dependency],

  -- | Build outcome, did the build phase work ok?
  buildOutcomeBuild    :: Outcome BuildPhase,

  -- | Build outcome, did building the docs work?
  buildOutcomeDocs     :: Outcome DocsPhase
}
  deriving (Show, Read)


data BuildPhase = BuildPhase {
  -- | Build outcome, did installing work ok?
  buildOutcomeInstall  :: Outcome InstallPhase
}
  deriving (Show, Read)


data DocsPhase = DocsPhase deriving (Show, Read)


data InstallPhase = InstallPhase deriving (Show, Read)


data Outcome a = OutcomeOk a | OutcomeFailed | OutcomeNotTried
  deriving (Show, Read)


writeBuildReport :: FilePath -> BuildReport -> IO ()
writeBuildReport file report = do
  createDirectoryIfMissing True (takeDirectory file)
  writeFile file $ show report


makeSuccessReport :: ConfiguredPackage -> (OS, String)
                  -> (CompilerFlavor, Maybe Version) -> BuildReport
makeSuccessReport (ConfiguredPackage pkgInfo flagAssignmnt pkgIds)
                  platform compiler =  
  BuildReport {
    buildPackage             = packageId pkgInfo,
    buildPlatform            = platform,
    buildCompiler            = compiler,
    buildOutcomeConfigure    = OutcomeOk $ ConfigurePhase {
    
makeFailureReport :: 
