{-# LANGUAGE RecordWildCards #-}

-- |
--
-- The layout of the .\/dist\/ directory where cabal keeps all of its state
-- and build artifacts.
--
module Distribution.Client.DistDirLayout (
    -- * 'DistDirLayout'
    DistDirLayout(..),
    DistDirParams(..),
    defaultDistDirLayout,

    -- * 'ProjectRoot'
    ProjectRoot(..),
    defaultProjectFile,

    -- * 'StoreDirLayout'
    StoreDirLayout(..),
    defaultStoreDirLayout,

    -- * 'CabalDirLayout'
    CabalDirLayout(..),
    mkCabalDirLayout,
    defaultCabalDirLayout
) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.FilePath

import Distribution.Client.Config
         ( defaultStoreDir, defaultLogsDir)
import Distribution.Package
         ( PackageId, PackageIdentifier, ComponentId, UnitId )
import Distribution.Compiler
import Distribution.Simple.Compiler
         ( PackageDB(..), PackageDBStack, OptimisationLevel(..) )
import Distribution.Types.ComponentName
import Distribution.Types.LibraryName
import Distribution.System


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

       -- | The root directory of the project. Many other files are relative to
       -- this location (e.g. the @cabal.project@ file).
       --
       distProjectRootDirectory     :: FilePath,

       -- | The @cabal.project@ file and related like @cabal.project.freeze@.
       -- The parameter is for the extension, like \"freeze\", or \"\" for the
       -- main file.
       --
       distProjectFile              :: String -> FilePath,

       -- | The \"dist\" directory, which is the root of where cabal keeps all
       -- its state including the build artifacts from each package we build.
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

       -- | The directory under dist where we download tarballs and source
       -- control repos to.
       --
       distDownloadSrcDirectory     :: FilePath,

       -- | The directory under dist where we put the unpacked sources of
       -- packages, in those cases where it makes sense to keep the build
       -- artifacts to reduce rebuild times.
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

       -- | The location that sdists are placed by default.
       distSdistFile                :: PackageId -> FilePath,
       distSdistDirectory           :: FilePath,

       distTempDirectory            :: FilePath,
       distBinDirectory             :: FilePath,

       distPackageDB                :: CompilerId -> PackageDB
     }


-- | The layout of a cabal nix-style store.
--
data StoreDirLayout = StoreDirLayout {
       storeDirectory         :: CompilerId -> FilePath,
       storePackageDirectory  :: CompilerId -> UnitId -> FilePath,
       storePackageDBPath     :: CompilerId -> FilePath,
       storePackageDB         :: CompilerId -> PackageDB,
       storePackageDBStack    :: CompilerId -> PackageDBStack,
       storeIncomingDirectory :: CompilerId -> FilePath,
       storeIncomingLock      :: CompilerId -> UnitId -> FilePath
     }


--TODO: move to another module, e.g. CabalDirLayout?
-- or perhaps rename this module to DirLayouts.

-- | The layout of the user-wide cabal directory, that is the @~/.cabal@ dir
-- on unix, and equivalents on other systems.
--
-- At the moment this is just a partial specification, but the idea is
-- eventually to cover it all.
--
data CabalDirLayout = CabalDirLayout {
       cabalStoreDirLayout        :: StoreDirLayout,

       cabalLogsDirectory         :: FilePath
     }


-- | Information about the root directory of the project.
--
-- It can either be an implicit project root in the current dir if no
-- @cabal.project@ file is found, or an explicit root if either
-- the file is found or the project root directory was specicied.
--
data ProjectRoot =
       -- | An implicit project root. It contains the absolute project
       -- root dir.
       ProjectRootImplicit FilePath

       -- | An explicit project root. It contains the absolute project
       -- root dir and the relative @cabal.project@ file (or explicit override)
     | ProjectRootExplicit FilePath FilePath

       -- | An explicit, absolute project root dir and an explicit, absolute
       -- @cabal.project@ file.
     | ProjectRootExplicitAbsolute FilePath FilePath
  deriving (Eq, Show)

defaultProjectFile :: FilePath
defaultProjectFile = "cabal.project"

-- | Make the default 'DistDirLayout' based on the project root dir and
-- optional overrides for the location of the @dist@ directory and the
-- @cabal.project@ file.
--
defaultDistDirLayout :: ProjectRoot    -- ^ the project root
                     -> Maybe FilePath -- ^ the @dist@ directory or default
                                       -- (absolute or relative to the root)
                     -> DistDirLayout
defaultDistDirLayout projectRoot mdistDirectory =
    DistDirLayout {..}
  where
    (projectRootDir, projectFile) = case projectRoot of
      ProjectRootImplicit dir              -> (dir, dir </> defaultProjectFile)
      ProjectRootExplicit dir file         -> (dir, dir </> file)
      ProjectRootExplicitAbsolute dir file -> (dir, file)

    distProjectRootDirectory :: FilePath
    distProjectRootDirectory = projectRootDir

    distProjectFile :: String -> FilePath
    distProjectFile ext      = projectFile <.> ext

    distDirectory :: FilePath
    distDirectory = distProjectRootDirectory
                </> fromMaybe "dist-newstyle" mdistDirectory
    --TODO: switch to just dist at some point, or some other new name

    distBuildRootDirectory :: FilePath
    distBuildRootDirectory   = distDirectory </> "build"

    distBuildDirectory :: DistDirParams -> FilePath
    distBuildDirectory params =
        distBuildRootDirectory </>
        prettyShow (distParamPlatform params) </>
        prettyShow (distParamCompilerId params) </>
        prettyShow (distParamPackageId params) </>
        (case distParamComponentName params of
            Nothing                  -> ""
            Just (CLibName LMainLibName) -> ""
            Just (CLibName (LSubLibName name)) -> "l" </> prettyShow name
            Just (CFLibName name)    -> "f" </> prettyShow name
            Just (CExeName name)     -> "x" </> prettyShow name
            Just (CTestName name)    -> "t" </> prettyShow name
            Just (CBenchName name)   -> "b" </> prettyShow name) </>
        (case distParamOptimization params of
            NoOptimisation -> "noopt"
            NormalOptimisation -> ""
            MaximumOptimisation -> "opt") </>
        (let uid_str = prettyShow (distParamUnitId params)
         in if uid_str == prettyShow (distParamComponentId params)
                then ""
                else uid_str)

    distUnpackedSrcRootDirectory :: FilePath
    distUnpackedSrcRootDirectory   = distDirectory </> "src"

    distUnpackedSrcDirectory :: PackageId -> FilePath
    distUnpackedSrcDirectory pkgid = distUnpackedSrcRootDirectory
                                      </> prettyShow pkgid
    -- we shouldn't get name clashes so this should be fine:
    distDownloadSrcDirectory :: FilePath
    distDownloadSrcDirectory       = distUnpackedSrcRootDirectory

    distProjectCacheDirectory :: FilePath
    distProjectCacheDirectory = distDirectory </> "cache"

    distProjectCacheFile :: FilePath -> FilePath
    distProjectCacheFile name = distProjectCacheDirectory </> name

    distPackageCacheDirectory :: DistDirParams -> FilePath
    distPackageCacheDirectory params = distBuildDirectory params </> "cache"

    distPackageCacheFile :: DistDirParams -> String -> FilePath
    distPackageCacheFile params name = distPackageCacheDirectory params </> name

    distSdistFile :: PackageIdentifier -> FilePath
    distSdistFile pid = distSdistDirectory </> prettyShow pid <.> "tar.gz"

    distSdistDirectory :: FilePath
    distSdistDirectory = distDirectory </> "sdist"

    distTempDirectory :: FilePath
    distTempDirectory = distDirectory </> "tmp"

    distBinDirectory :: FilePath
    distBinDirectory = distDirectory </> "bin"

    distPackageDBPath :: CompilerId -> FilePath
    distPackageDBPath compid = distDirectory </> "packagedb" </> prettyShow compid

    distPackageDB :: CompilerId -> PackageDB
    distPackageDB = SpecificPackageDB . distPackageDBPath


defaultStoreDirLayout :: FilePath -> StoreDirLayout
defaultStoreDirLayout storeRoot =
    StoreDirLayout {..}
  where
    storeDirectory :: CompilerId -> FilePath
    storeDirectory compid =
      storeRoot </> prettyShow compid

    storePackageDirectory :: CompilerId -> UnitId -> FilePath
    storePackageDirectory compid ipkgid =
      storeDirectory compid </> prettyShow ipkgid

    storePackageDBPath :: CompilerId -> FilePath
    storePackageDBPath compid =
      storeDirectory compid </> "package.db"

    storePackageDB :: CompilerId -> PackageDB
    storePackageDB compid =
      SpecificPackageDB (storePackageDBPath compid)

    storePackageDBStack :: CompilerId -> PackageDBStack
    storePackageDBStack compid =
      [GlobalPackageDB, storePackageDB compid]

    storeIncomingDirectory :: CompilerId -> FilePath
    storeIncomingDirectory compid =
      storeDirectory compid </> "incoming"

    storeIncomingLock :: CompilerId -> UnitId -> FilePath
    storeIncomingLock compid unitid =
      storeIncomingDirectory compid </> prettyShow unitid <.> "lock"


defaultCabalDirLayout :: IO CabalDirLayout
defaultCabalDirLayout =
    mkCabalDirLayout Nothing Nothing

mkCabalDirLayout :: Maybe FilePath -- ^ Store directory. Must be absolute
                 -> Maybe FilePath -- ^ Log directory
                 -> IO CabalDirLayout
mkCabalDirLayout mstoreDir mlogDir = do
    cabalStoreDirLayout <-
      defaultStoreDirLayout <$> maybe defaultStoreDir pure mstoreDir
    cabalLogsDirectory <-
      maybe defaultLogsDir pure mlogDir
    pure $ CabalDirLayout {..}
