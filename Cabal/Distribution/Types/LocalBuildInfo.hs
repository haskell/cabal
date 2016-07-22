{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}

module Distribution.Types.LocalBuildInfo (
    LocalBuildInfo(..),
    localComponentId,
    localUnitId,
    localCompatPackageKey,
    externalPackageDeps,
  ) where

import Distribution.Types.PackageDescription
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ComponentEnabledSpec

import Distribution.Simple.InstallDirs hiding (absoluteInstallDirs,
                                               prefixRelativeInstallDirs,
                                               substPathTemplate, )
import Distribution.Simple.Program
import Distribution.PackageDescription
import Distribution.Package
import Distribution.Simple.Compiler
import Distribution.Simple.PackageIndex
import Distribution.Simple.Setup
import Distribution.Text
import Distribution.System

import Distribution.Compat.Binary (Binary)
import Data.List (nub)
import GHC.Generics (Generic)

-- | Data cached after configuration step.  See also
-- 'Distribution.Simple.Setup.ConfigFlags'.
data LocalBuildInfo = LocalBuildInfo {
        configFlags   :: ConfigFlags,
        -- ^ Options passed to the configuration step.
        -- Needed to re-run configuration when .cabal is out of date
        flagAssignment :: FlagAssignment,
        -- ^ The final set of flags which were picked for this package
        componentEnabledSpec :: ComponentEnabledSpec,
        -- ^ What components were enabled during configuration, and why.
        extraConfigArgs     :: [String],
        -- ^ Extra args on the command line for the configuration step.
        -- Needed to re-run configuration when .cabal is out of date
        installDirTemplates :: InstallDirTemplates,
                -- ^ The installation directories for the various different
                -- kinds of files
        --TODO: inplaceDirTemplates :: InstallDirs FilePath
        compiler      :: Compiler,
                -- ^ The compiler we're building with
        hostPlatform  :: Platform,
                -- ^ The platform we're building for
        buildDir      :: FilePath,
                -- ^ Where to build the package.
        componentsConfigs   :: [(ComponentLocalBuildInfo, [UnitId])],
                -- ^ All the components to build, ordered by topological
                -- sort, and with their INTERNAL dependencies over the
                -- intrapackage dependency graph.
                -- TODO: this is assumed to be short; otherwise we want
                -- some sort of ordered map.
        installedPkgs :: InstalledPackageIndex,
                -- ^ All the info about the installed packages that the
                -- current package depends on (directly or indirectly).
                -- Does NOT include internal dependencies.
        pkgDescrFile  :: Maybe FilePath,
                -- ^ the filename containing the .cabal file, if available
        localPkgDescr :: PackageDescription,
                -- ^ The resolved package description, that does not contain
                -- any conditionals.
        withPrograms  :: ProgramConfiguration, -- ^Location and args for all programs
        withPackageDB :: PackageDBStack,  -- ^What package database to use, global\/user
        withVanillaLib:: Bool,  -- ^Whether to build normal libs.
        withProfLib   :: Bool,  -- ^Whether to build profiling versions of libs.
        withSharedLib :: Bool,  -- ^Whether to build shared versions of libs.
        withDynExe    :: Bool,  -- ^Whether to link executables dynamically
        withProfExe   :: Bool,  -- ^Whether to build executables for profiling.
        withProfLibDetail :: ProfDetailLevel, -- ^Level of automatic profile detail.
        withProfExeDetail :: ProfDetailLevel, -- ^Level of automatic profile detail.
        withOptimization :: OptimisationLevel, -- ^Whether to build with optimization (if available).
        withDebugInfo :: DebugInfoLevel, -- ^Whether to emit debug info (if available).
        withGHCiLib   :: Bool,  -- ^Whether to build libs suitable for use with GHCi.
        splitObjs     :: Bool,  -- ^Use -split-objs with GHC, if available
        stripExes     :: Bool,  -- ^Whether to strip executables during install
        stripLibs     :: Bool,  -- ^Whether to strip libraries during install
        progPrefix    :: PathTemplate, -- ^Prefix to be prepended to installed executables
        progSuffix    :: PathTemplate, -- ^Suffix to be appended to installed executables
        relocatable   :: Bool --  ^Whether to build a relocatable package
  } deriving (Generic, Read, Show)

instance Binary LocalBuildInfo

-- TODO: Get rid of these functions, as much as possible.  They are
-- a bit useful in some cases, but you should be very careful!

-- | Extract the 'ComponentId' from the public library component of a
-- 'LocalBuildInfo' if it exists, or make a fake component ID based
-- on the package ID.
localComponentId :: LocalBuildInfo -> ComponentId
localComponentId lbi
    = case localUnitId lbi of
        SimpleUnitId cid -> cid

-- | Extract the 'UnitId' from the library component of a
-- 'LocalBuildInfo' if it exists, or make a fake unit ID based on
-- the package ID.
localUnitId :: LocalBuildInfo -> UnitId
localUnitId lbi
    = case maybeGetDefaultLibraryLocalBuildInfo lbi of
        Just LibComponentLocalBuildInfo { componentUnitId = uid } -> uid
        -- Something fake:
        _ -> mkLegacyUnitId (package (localPkgDescr lbi))

-- | Extract the compatibility package key from the public library component of a
-- 'LocalBuildInfo' if it exists, or make a fake package key based
-- on the package ID.
localCompatPackageKey :: LocalBuildInfo -> String
localCompatPackageKey lbi =
    case maybeGetDefaultLibraryLocalBuildInfo lbi of
        Just LibComponentLocalBuildInfo { componentCompatPackageKey = pk } -> pk
        -- Something fake:
        _ -> display (package (localPkgDescr lbi))

-- TODO: Private for now!  We will refactor this.
maybeGetDefaultLibraryLocalBuildInfo
    :: LocalBuildInfo
    -> Maybe ComponentLocalBuildInfo
maybeGetDefaultLibraryLocalBuildInfo lbi =
    case [ clbi
         | (clbi@LibComponentLocalBuildInfo{}, _) <- componentsConfigs lbi
         , componentIsPublic clbi
         ] of
      [clbi] -> Just clbi
      _ -> Nothing

-- | External package dependencies for the package as a whole. This is the
-- union of the individual 'componentPackageDeps', less any internal deps.
externalPackageDeps :: LocalBuildInfo -> [(UnitId, PackageId)]
externalPackageDeps lbi =
    -- TODO:  what about non-buildable components?
    nub [ (ipkgid, pkgid)
        | (clbi,_)      <- componentsConfigs lbi
        , (ipkgid, pkgid) <- componentPackageDeps clbi
        , not (internal ipkgid) ]
  where
    -- True if this dependency is an internal one (depends on the library
    -- defined in the same package).
    internal ipkgid = any ((==ipkgid) . componentUnitId . fst) (componentsConfigs lbi)
