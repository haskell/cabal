{-# LANGUAGE RecordWildCards #-}

-- | 
--
-- The layout of the .\/dist\/ directory where cabal keeps all of it's state
-- and build artifacts.
--
module Distribution.Client.DistDirLayout where

import System.FilePath
import Distribution.Package
         ( PackageId )
import Distribution.Compiler
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Text
import Distribution.Client.Types
         ( InstalledPackageId )



-- | The layout of the project state directory. Traditionally this has been
-- called the @dist@ directory.
--
data DistDirLayout = DistDirLayout {

        -- | The dist directory, which is the root of where cabal keeps all its
       -- state including the build artifacts from each package we build.
       --
       distDirectory                :: FilePath,

       -- | The directory under dist where we keep the build artifacts for a
       -- package we're building from a local directory.
       --
       -- This uses a 'PackageId' not just a 'PackageName' because technically
       -- we can have multiple instances of the same package in a solution
       -- (e.g. setup deps).
       --
       distBuildDirectory           :: PackageId -> FilePath,
       distBuildRootDirectory       :: FilePath,

       -- | The directory under dist where we put the unpacked sources of
       -- packages, in those cases where it makes sense to keep the build
       -- artifacts to reduce rebuild times. These can be tarballs or could be
       -- scm repos.
       --
       distUnpackedSrcDirectory     :: PackageId -> FilePath,
       distUnpackedSrcRootDirectory :: FilePath,

       -- | The location for project-wide cache files (e.g. state used in
       -- incremental rebuilds).
       --
       distProjectCacheFile         :: String -> FilePath,
       distProjectCacheDirectory    :: FilePath,

       -- | The location for package-specific cache files (e.g. state used in
       -- incremental rebuilds).
       --
       distPackageCacheFile         :: PackageId -> String -> FilePath,
       distPackageCacheDirectory    :: PackageId -> FilePath,

       distTempDirectory            :: FilePath,
       distBinDirectory             :: FilePath,

       distPackageDB                :: CompilerId -> PackageDB
     }



--TODO: move to another module, e.g. CabalDirLayout?
data CabalDirLayout = CabalDirLayout {
       cabalStoreDirectory        :: CompilerId -> FilePath,
       cabalStorePackageDirectory :: CompilerId -> InstalledPackageId
                                                -> FilePath,
       cabalStorePackageDBPath    :: CompilerId -> FilePath,
       cabalStorePackageDB        :: CompilerId -> PackageDB,

       cabalPackageCacheDirectory :: FilePath,
       cabalLogsDirectory         :: FilePath,
       cabalWorldFile             :: FilePath
     }


defaultDistDirLayout :: FilePath -> DistDirLayout
defaultDistDirLayout projectRootDirectory =
    DistDirLayout {..}
  where
    distDirectory = projectRootDirectory </> "dist-newstyle"
    --TODO: switch to just dist at some point, or some other new name

    distBuildRootDirectory   = distDirectory </> "build"
    distBuildDirectory pkgid = distBuildRootDirectory </> display pkgid

    distUnpackedSrcRootDirectory   = distDirectory </> "src"
    distUnpackedSrcDirectory pkgid = distUnpackedSrcRootDirectory
                                      </> display pkgid

    distProjectCacheDirectory = distDirectory </> "cache"
    distProjectCacheFile name = distProjectCacheDirectory </> name

    distPackageCacheDirectory pkgid = distBuildDirectory pkgid </> "cache"
    distPackageCacheFile pkgid name = distPackageCacheDirectory pkgid </> name

    distTempDirectory = distDirectory </> "tmp"

    distBinDirectory = distDirectory </> "bin"

    distPackageDBPath compid = distDirectory </> "packagedb" </> display compid
    distPackageDB = SpecificPackageDB . distPackageDBPath



defaultCabalDirLayout :: FilePath -> CabalDirLayout
defaultCabalDirLayout cabalDir =
    CabalDirLayout {..}
  where

    cabalStoreDirectory compid =
      cabalDir </> "store" </> display compid

    cabalStorePackageDirectory compid ipkgid = 
      cabalStoreDirectory compid </> display ipkgid

    cabalStorePackageDBPath compid =
      cabalStoreDirectory compid </> "package.db"

    cabalStorePackageDB =
      SpecificPackageDB . cabalStorePackageDBPath

    cabalPackageCacheDirectory = cabalDir </> "packages"

    cabalLogsDirectory = cabalDir </> "logs"

    cabalWorldFile = cabalDir </> "world"

