{-# LANGUAGE RecordWildCards #-}

-- | 
--
-- The layout of the .\/dist\/ directory where cabal keeps all of it's state
-- and build artifacts.
--
module Distribution.Client.DistDirLayout (
    -- 'DistDirLayout'
    DistDirLayout(..),
    DistDirParams(..),
    defaultDistDirLayout,

    -- * 'CabalDirLayout'
    CabalDirLayout(..),
    defaultCabalDirLayout,
) where

import System.FilePath
import Distribution.Package
         ( PackageId, ComponentId, UnitId )
import Distribution.Compiler
import Distribution.Simple.Compiler (PackageDB(..), OptimisationLevel(..))
import Distribution.Text
import Distribution.Types.ComponentName
import Distribution.System
import Distribution.Client.Types
         ( InstalledPackageId )

-- | Information which can be used to construct the path to
-- the build directory of a build.  This is LESS fine-grained
-- than what goes into the hashed 'InstalledPackageId',
-- and for good reason: we don't want this path to change if
-- the user, say, adds a dependency to their project.
data DistDirParams = DistDirParams {
    distParamUnitId         :: UnitId,
    distParamPackageId      :: PackageId,
    distParamComponentId    :: ComponentId,
    distParamComponentName  :: Maybe ComponentName,
    distParamCompilerId     :: CompilerId,
    distParamPlatform       :: Platform,
    distParamOptimization   :: OptimisationLevel
    -- TODO (see #3343):
    --  Flag assignments
    --  Optimization
    }


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
       -- This uses a 'UnitId' not just a 'PackageName' because technically
       -- we can have multiple instances of the same package in a solution
       -- (e.g. setup deps).
       --
       distBuildDirectory           :: DistDirParams -> FilePath,
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
       distPackageCacheFile         :: DistDirParams -> String -> FilePath,
       distPackageCacheDirectory    :: DistDirParams -> FilePath,

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
    distBuildDirectory params =
        distBuildRootDirectory </>
        display (distParamPlatform params) </>
        display (distParamCompilerId params) </>
        display (distParamPackageId params) </>
        (case fmap componentNameString (distParamComponentName params) of
            Nothing          -> ""
            Just Nothing     -> ""
            Just (Just name) -> "c" </> display name) </>
        (case distParamOptimization params of
            NoOptimisation -> "noopt"
            NormalOptimisation -> ""
            MaximumOptimisation -> "opt") </>
        (let uid_str = display (distParamUnitId params)
         in if uid_str == display (distParamComponentId params)
                then ""
                else uid_str)

    distUnpackedSrcRootDirectory   = distDirectory </> "src"
    distUnpackedSrcDirectory pkgid = distUnpackedSrcRootDirectory
                                      </> display pkgid

    distProjectCacheDirectory = distDirectory </> "cache"
    distProjectCacheFile name = distProjectCacheDirectory </> name

    distPackageCacheDirectory params = distBuildDirectory params </> "cache"
    distPackageCacheFile params name = distPackageCacheDirectory params </> name

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

    cabalLogsDirectory = cabalDir </> "logs"

    cabalWorldFile = cabalDir </> "world"

