{-# LANGUAGE RecordWildCards #-}

-- | 
--
-- The layout of the .\/dist\/ directory where cabal keeps all of it's state
-- and build artifacts.
--
module Distribution.Client.DistDirLayout where

import System.FilePath
import Distribution.Package
import Distribution.Compiler
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Text

--  Our state directory looks like this
--

-- | The dist directory, which is the root of where cabal keeps all its state
-- including the build artifacts from each package we build.
--
distDirectory :: FilePath
distDirectory = "." </> "dist-newstyle"

-- | The directory under dist where we keep the build artifacts for a package
-- we're building from a local directory.
--
-- This uses a package id not just a package name because technically we can
-- have multiple instances of the same package in a solution (e.g. setup deps).
--
distBuildDirectory :: PackageId -> FilePath
distBuildDirectory pkgid = distBuildRootDirectory </> display pkgid

distBuildRootDirectory :: FilePath
distBuildRootDirectory = distDirectory </> "build"

-- | The directory under dist where we put the unpacked sources of packages,
-- in those cases where it makes sense to keep the build artifacts to reduce
-- rebuild times. These can be tarballs or could be scm repos.
--
distUnpackedSrcDirectory :: PackageId -> FilePath
distUnpackedSrcDirectory pkgid = distUnpackedSrcRootDirectory </> display pkgid

distUnpackedSrcRootDirectory :: FilePath
distUnpackedSrcRootDirectory = distDirectory </> "src"

-- | The location of the file status cache used to track the files in a local
-- unpacked source directory.
--
-- > <dist>/build/<pkgid>/filestatus.cache
--
distUnpackedSrcFileStatusCache :: PackageId -> FilePath
distUnpackedSrcFileStatusCache pkgid =
    distBuildDirectory pkgid </> "filestatus.cache"

distUnpackedSrcConfigCache :: PackageId -> FilePath
distUnpackedSrcConfigCache pkgid =
    distBuildDirectory pkgid </> "config.cache"

distSolverInputsCache :: FilePath
distSolverInputsCache =
    distBuildRootDirectory </> "solver.cache"

distInstallPlanCache :: FilePath
distInstallPlanCache =
    distBuildRootDirectory </> "plan.cache"

distTempDirectory :: FilePath
distTempDirectory = distDirectory </> "tmp"

distBinDirectory :: FilePath
distBinDirectory = distDirectory </> "bin"

distPackageDB :: FilePath
distPackageDB = distDirectory </> "package.db"


-----

--TODO: move to another module, e.g. CabalDirLayout


data CabalDirLayout = CabalDirLayout {
       cabalStoreDirectory      :: CompilerId -> FilePath,
       cabalStorePackageDBPath  :: CompilerId -> FilePath,
       cabalStorePackageDB      :: CompilerId -> PackageDB
     }

defaultCabalDirLayout :: FilePath -> CabalDirLayout
defaultCabalDirLayout cabalDir =
    CabalDirLayout {..}
  where

    cabalStoreDirectory compid = 
      cabalDir </> "store" </> display compid

    cabalStorePackageDBPath compid =
      cabalDir </> "store" </> display compid </> "package.db"

    cabalStorePackageDB =
      SpecificPackageDB . cabalStorePackageDBPath

