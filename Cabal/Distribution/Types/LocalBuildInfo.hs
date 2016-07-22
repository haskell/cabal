{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}

module Distribution.Types.LocalBuildInfo (
    -- * The type

    LocalBuildInfo(..),

    -- * Convenience accessors

    localComponentId,
    localUnitId,
    localCompatPackageKey,

    -- * Build targets of the 'LocalBuildInfo'.

    mkTargetInfo,

    componentNameTargets,
    allTargetsInBuildOrder,
    withAllTargetsInBuildOrder,
    neededTargetsInBuildOrder,
    withNeededTargetsInBuildOrder,

    -- * Backwards compatibility.

    componentsConfigs,
    externalPackageDeps,
  ) where

import Distribution.Types.PackageDescription
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ComponentEnabledSpec
import Distribution.Types.TargetInfo

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

import Distribution.Compat.Graph (Graph)
import qualified Distribution.Compat.Graph as Graph
import Distribution.Compat.Binary (Binary)
import Data.List (nub, intercalate)
import qualified Data.Map as Map
import Data.Map (Map)
import GHC.Generics (Generic)
import Data.Maybe (mapMaybe)

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
        componentGraph :: Graph ComponentLocalBuildInfo,
                -- ^ All the components to build, ordered by topological
                -- sort, and with their INTERNAL dependencies over the
                -- intrapackage dependency graph.
                -- TODO: this is assumed to be short; otherwise we want
                -- some sort of ordered map.
        componentNameMap :: Map ComponentName [ComponentLocalBuildInfo],
                -- ^ A map from component name to all matching
                -- components.  These coincide with 'componentGraph'
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

-------------------------------------------------------------------------------
-- Accessor functions

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
localUnitId lbi = 
    case componentNameTargets lbi CLibName of
        [target] | LibComponentLocalBuildInfo { componentUnitId = uid }
                     <- targetCLBI target
                -> uid
        _ -> mkLegacyUnitId (package (localPkgDescr lbi))

-- | Extract the compatibility package key from the public library component of a
-- 'LocalBuildInfo' if it exists, or make a fake package key based
-- on the package ID.
localCompatPackageKey :: LocalBuildInfo -> String
localCompatPackageKey lbi =
    case componentNameTargets lbi CLibName of
        [target] | LibComponentLocalBuildInfo { componentCompatPackageKey = pk }
                    <- targetCLBI target
                -> pk
        _ -> display (package (localPkgDescr lbi))

-- | Generate a default 'TargetInfo' from a 'ComponentLocalBuildInfo'.
-- The idea is to call this once, and then use 'TargetInfo' everywhere
-- else.
mkTargetInfo :: LocalBuildInfo -> ComponentLocalBuildInfo -> TargetInfo
mkTargetInfo lbi clbi =
    TargetInfo {
        targetCLBI = clbi,
        targetComponent = getComponent (localPkgDescr lbi)
                                       (componentLocalName clbi)
     }

-- | Return all 'TargetInfo's associated with 'ComponentName'.
-- In the presence of Backpack there may be more than one!
componentNameTargets :: LocalBuildInfo -> ComponentName -> [TargetInfo]
componentNameTargets lbi cname =
    case Map.lookup cname (componentNameMap lbi) of
        Just clbis -> map (mkTargetInfo lbi) clbis
        Nothing -> []

-- TODO: Maybe cache topsort (Graph can do this)

-- | Return the list of default 'TargetInfo's associated with a
-- configured package, in the order they need to be built.
allTargetsInBuildOrder :: LocalBuildInfo -> [TargetInfo]
allTargetsInBuildOrder lbi
    = map (mkTargetInfo lbi) (Graph.revTopSort (componentGraph lbi))

-- | Execute @f@ for every 'TargetInfo' in the package, respecting the
-- build dependency order.  (TODO: We should use Shake!)
withAllTargetsInBuildOrder :: LocalBuildInfo -> (TargetInfo -> IO ()) -> IO ()
withAllTargetsInBuildOrder lbi f
    = sequence_ [ f target | target <- allTargetsInBuildOrder lbi ]

-- | Return the list of all targets needed to build the @uids@, in
-- the order they need to be built.
neededTargetsInBuildOrder :: LocalBuildInfo -> [UnitId] -> [TargetInfo]
neededTargetsInBuildOrder lbi uids =
  case Graph.closure (componentGraph lbi) uids of
    Nothing -> error $ "localBuildPlan: missing uids " ++ intercalate ", " (map display uids)
    Just clos -> map (mkTargetInfo lbi) (Graph.revTopSort (Graph.fromList clos))

-- | Execute @f@ for every 'TargetInfo' needed to build @uid@s, respecting
-- the build dependency order.
withNeededTargetsInBuildOrder :: LocalBuildInfo -> [UnitId] -> (TargetInfo -> IO ()) -> IO ()
withNeededTargetsInBuildOrder lbi uids f
    = sequence_ [ f target | target <- neededTargetsInBuildOrder lbi uids ]

-------------------------------------------------------------------------------
-- Backwards compatibility

{-# DEPRECATED componentsConfigs "Use 'componentGraph' instead; you can get a list of 'ComponentLocalBuildInfo' with 'Distribution.Compat.Graph.toList'. There's not a good way to get the list of 'ComponentName's the 'ComponentLocalBuildInfo' depends on because this query doesn't make sense; the graph is indexed by 'UnitId' not 'ComponentName'.  Given a 'UnitId' you can lookup the 'ComponentLocalBuildInfo' ('getCLBI') and then get the 'ComponentName' ('componentLocalName]). To be removed in Cabal 2.2" #-}
componentsConfigs :: LocalBuildInfo -> [(ComponentName, ComponentLocalBuildInfo, [ComponentName])]
componentsConfigs lbi =
    [ (componentLocalName clbi,
       clbi,
       mapMaybe (fmap componentLocalName . flip Graph.lookup g)
                (componentInternalDeps clbi))
    | clbi <- Graph.toList g ]
  where
    g = componentGraph lbi

-- | External package dependencies for the package as a whole. This is the
-- union of the individual 'componentPackageDeps', less any internal deps.
{-# DEPRECATED externalPackageDeps "You almost certainly don't want this function, which agglomerates the dependencies of ALL enabled components.  If you're using this to write out information on your dependencies, read off the dependencies directly from the actual component in question.  To be removed in Cabal 2.2" #-}
externalPackageDeps :: LocalBuildInfo -> [(UnitId, PackageId)]
externalPackageDeps lbi =
    -- TODO:  what about non-buildable components?
    nub [ (ipkgid, pkgid)
        | clbi            <- Graph.toList (componentGraph lbi)
        , (ipkgid, pkgid) <- componentPackageDeps clbi
        , not (internal ipkgid) ]
  where
    -- True if this dependency is an internal one (depends on the library
    -- defined in the same package).
    internal ipkgid = any ((==ipkgid) . componentUnitId) (Graph.toList (componentGraph lbi))
