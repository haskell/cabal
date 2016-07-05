{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.LocalBuildInfo
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Once a package has been configured we have resolved conditionals and
-- dependencies, configured the compiler and other needed external programs.
-- The 'LocalBuildInfo' is used to hold all this information. It holds the
-- install dirs, the compiler, the exact package dependencies, the configured
-- programs, the package database to use and a bunch of miscellaneous configure
-- flags. It gets saved and reloaded from a file (@dist\/setup-config@). It gets
-- passed in to very many subsequent build actions.

module Distribution.Simple.LocalBuildInfo (
        LocalBuildInfo(..),
        externalPackageDeps,
        localComponentId,
        localUnitId,
        localCompatPackageKey,

        -- * Buildable package components
        Component(..),
        ComponentName(..),
        defaultLibName,
        showComponentName,
        componentNameString,
        ComponentLocalBuildInfo(..),
        getLocalComponent,
        componentComponentId,
        componentBuildDir,
        foldComponent,
        componentName,
        componentBuildInfo,
        componentBuildable,
        pkgComponents,
        pkgBuildableComponents,
        lookupComponent,
        getComponent,
        maybeGetDefaultLibraryLocalBuildInfo,
        maybeGetComponentLocalBuildInfo,
        getComponentLocalBuildInfo,
        allComponentsInBuildOrder,
        componentsInBuildOrder,
        checkComponentsCyclic,
        depLibraryPaths,

        withAllComponentsInBuildOrder,
        withComponentsInBuildOrder,
        withComponentsLBI,
        withLibLBI,
        withExeLBI,
        withTestLBI,
        testLBIs,

        -- * Installation directories
        module Distribution.Simple.InstallDirs,
        absoluteInstallDirs, prefixRelativeInstallDirs,
        absoluteComponentInstallDirs, prefixRelativeComponentInstallDirs,
        substPathTemplate
  ) where


import Distribution.Simple.InstallDirs hiding (absoluteInstallDirs,
                                               prefixRelativeInstallDirs,
                                               substPathTemplate, )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.Program
import Distribution.PackageDescription
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.Package
import Distribution.Simple.Compiler
import Distribution.Simple.PackageIndex
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Text
import Distribution.System

import Data.Array ((!))
import Distribution.Compat.Binary (Binary)
import Data.Graph
import Data.List (nub, find, stripPrefix)
import Data.Maybe
import Data.Tree  (flatten)
import GHC.Generics (Generic)
import System.FilePath

import System.Directory (doesDirectoryExist, canonicalizePath)

-- | Data cached after configuration step.  See also
-- 'Distribution.Simple.Setup.ConfigFlags'.
data LocalBuildInfo = LocalBuildInfo {
        configFlags   :: ConfigFlags,
        -- ^ Options passed to the configuration step.
        -- Needed to re-run configuration when .cabal is out of date
        flagAssignment :: FlagAssignment,
        -- ^ The final set of flags which were picked for this package
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

-- -----------------------------------------------------------------------------
-- Source-representation of buildable components

data Component = CLib   Library
               | CExe   Executable
               | CTest  TestSuite
               | CBench Benchmark
               deriving (Show, Eq, Read)

-- | This gets the 'String' component name. In fact, it is
-- guaranteed to uniquely identify a component, returning
-- @Nothing@ if the 'ComponentName' was for the public
-- library (which CAN conflict with an executable name.)
componentNameString :: PackageName -> ComponentName -> Maybe String
componentNameString (PackageName pkg_name) (CLibName n) | pkg_name == n = Nothing
componentNameString _ (CLibName   n) = Just n
componentNameString _ (CExeName   n) = Just n
componentNameString _ (CTestName  n) = Just n
componentNameString _ (CBenchName n) = Just n

showComponentName :: ComponentName -> String
showComponentName (CLibName   name) = "library '" ++ name ++ "'"
showComponentName (CExeName   name) = "executable '" ++ name ++ "'"
showComponentName (CTestName  name) = "test suite '" ++ name ++ "'"
showComponentName (CBenchName name) = "benchmark '" ++ name ++ "'"

foldComponent :: (Library -> a)
              -> (Executable -> a)
              -> (TestSuite -> a)
              -> (Benchmark -> a)
              -> Component
              -> a
foldComponent f _ _ _ (CLib   lib) = f lib
foldComponent _ f _ _ (CExe   exe) = f exe
foldComponent _ _ f _ (CTest  tst) = f tst
foldComponent _ _ _ f (CBench bch) = f bch

componentBuildInfo :: Component -> BuildInfo
componentBuildInfo =
  foldComponent libBuildInfo buildInfo testBuildInfo benchmarkBuildInfo

componentName :: Component -> ComponentName
componentName =
  foldComponent (CLibName . libName)
                (CExeName . exeName)
                (CTestName . testName)
                (CBenchName . benchmarkName)

-- | All the components in the package (libs, exes, or test suites).
--
pkgComponents :: PackageDescription -> [Component]
pkgComponents pkg =
    [ CLib  lib | lib <- libraries pkg ]
 ++ [ CExe  exe | exe <- executables pkg ]
 ++ [ CTest tst | tst <- testSuites  pkg ]
 ++ [ CBench bm | bm  <- benchmarks  pkg ]

-- | All the components in the package that are buildable and enabled.
-- Thus this excludes non-buildable components and test suites or benchmarks
-- that have been disabled.
--
pkgBuildableComponents :: PackageDescription -> [Component]
pkgBuildableComponents = filter componentBuildable . pkgComponents

componentBuildable :: Component -> Bool
componentBuildable = buildable . componentBuildInfo

lookupComponent :: PackageDescription -> ComponentName -> Maybe Component
lookupComponent pkg (CLibName "") = lookupComponent pkg (defaultLibName (package pkg))
lookupComponent pkg (CLibName name) =
    fmap CLib $ find ((name ==) . libName) (libraries pkg)
lookupComponent pkg (CExeName name) =
    fmap CExe $ find ((name ==) . exeName) (executables pkg)
lookupComponent pkg (CTestName name) =
    fmap CTest $ find ((name ==) . testName) (testSuites pkg)
lookupComponent pkg (CBenchName name) =
    fmap CBench $ find ((name ==) . benchmarkName) (benchmarks pkg)

getComponent :: PackageDescription -> ComponentName -> Component
getComponent pkg cname =
    case lookupComponent pkg cname of
      Just cpnt -> cpnt
      Nothing   -> missingComponent
  where
    missingComponent =
      error $ "internal error: the package description contains no "
           ++ "component corresponding to " ++ show cname

-- -----------------------------------------------------------------------------
-- Configuration information of buildable components

data ComponentLocalBuildInfo
  = LibComponentLocalBuildInfo {
    -- | It would be very convenient to store the literal Library here,
    -- but if we do that, it will get serialized (via the Binary)
    -- instance twice.  So instead we just provide the ComponentName,
    -- which can be used to find the Component in the
    -- PackageDescription.  NB: eventually, this will NOT uniquely
    -- identify the ComponentLocalBuildInfo.
    componentLocalName :: ComponentName,
    -- | Resolved internal and external package dependencies for this component.
    -- The 'BuildInfo' specifies a set of build dependencies that must be
    -- satisfied in terms of version ranges. This field fixes those dependencies
    -- to the specific versions available on this machine for this compiler.
    componentPackageDeps :: [(UnitId, PackageId)],
    -- | The computed 'UnitId' which uniquely identifies this
    -- component.
    componentUnitId :: UnitId,
    -- | Compatibility "package key" that we pass to older versions of GHC.
    componentCompatPackageKey :: String,
    -- | Compatability "package name" that we register this component as.
    componentCompatPackageName :: PackageName,
    -- | A list of exposed modules (either defined in this component,
    -- or reexported from another component.)
    componentExposedModules :: [Installed.ExposedModule],
    -- | Convenience field, specifying whether or not this is the
    -- "public library" that has the same name as the package.
    componentIsPublic :: Bool,
    -- | The set of packages that are brought into scope during
    -- compilation, including a 'ModuleRenaming' which may used
    -- to hide or rename modules.  This is what gets translated into
    -- @-package-id@ arguments.  This is a modernized version of
    -- 'componentPackageDeps', which is kept around for BC purposes.
    componentIncludes :: [(UnitId, ModuleRenaming)]
  }
  | ExeComponentLocalBuildInfo {
    componentLocalName :: ComponentName,
    componentUnitId :: UnitId,
    componentPackageDeps :: [(UnitId, PackageId)],
    componentIncludes :: [(UnitId, ModuleRenaming)]
  }
  | TestComponentLocalBuildInfo {
    componentLocalName :: ComponentName,
    componentUnitId :: UnitId,
    componentPackageDeps :: [(UnitId, PackageId)],
    componentIncludes :: [(UnitId, ModuleRenaming)]
  }
  | BenchComponentLocalBuildInfo {
    componentLocalName :: ComponentName,
    componentUnitId :: UnitId,
    componentPackageDeps :: [(UnitId, PackageId)],
    componentIncludes :: [(UnitId, ModuleRenaming)]
  }
  deriving (Generic, Read, Show)

instance Binary ComponentLocalBuildInfo

getLocalComponent :: PackageDescription -> ComponentLocalBuildInfo -> Component
getLocalComponent pkg_descr clbi = getComponent pkg_descr (componentLocalName clbi)

componentComponentId :: ComponentLocalBuildInfo -> ComponentId
componentComponentId clbi = case componentUnitId clbi of
                                SimpleUnitId cid -> cid

componentBuildDir :: LocalBuildInfo -> ComponentLocalBuildInfo -> FilePath
componentBuildDir lbi LibComponentLocalBuildInfo{ componentIsPublic = True }
    = buildDir lbi
-- For now, we assume that libraries/executables/test-suites/benchmarks
-- are only ever built once.  With Backpack, we need a special case for
-- libraries so that we can handle building them multiple times.
componentBuildDir lbi clbi
    = buildDir lbi </> case componentLocalName clbi of
                        CLibName s   -> s
                        CExeName s   -> s
                        CTestName s  -> s
                        CBenchName s -> s

getComponentLocalBuildInfo :: LocalBuildInfo -> ComponentName -> ComponentLocalBuildInfo
getComponentLocalBuildInfo lbi cname =
    case maybeGetComponentLocalBuildInfo lbi cname of
      Just clbi -> clbi
      Nothing   ->
          error $ "internal error: there is no configuration data "
               ++ "for component " ++ show cname

maybeGetComponentLocalBuildInfo
    :: LocalBuildInfo -> ComponentName -> Maybe ComponentLocalBuildInfo
maybeGetComponentLocalBuildInfo lbi cname =
    case [ clbi
         | (clbi, _) <- componentsConfigs lbi
         , cid <- componentNameToUnitIds lbi cname
         , cid == componentUnitId clbi ] of
      [clbi] -> Just clbi
      _      -> Nothing

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

componentNameToUnitIds :: LocalBuildInfo -> ComponentName -> [UnitId]
componentNameToUnitIds lbi cname =
    [ componentUnitId clbi
    | (clbi, _) <- componentsConfigs lbi
    , componentName (getLocalComponent (localPkgDescr lbi) clbi) == cname ]

-- | Perform the action on each buildable 'library' in the package
-- description.  Extended version of 'withLib' that also gives
-- corresponding build info.
withLibLBI :: PackageDescription -> LocalBuildInfo
           -> (Library -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withLibLBI pkg lbi f =
    sequence_
        [ f lib clbi
        | (clbi@LibComponentLocalBuildInfo{}, _) <- componentsConfigs lbi
        , CLib lib <- [getComponent pkg (componentLocalName clbi)] ]

-- | Perform the action on each buildable 'Executable' in the package
-- description.  Extended version of 'withExe' that also gives corresponding
-- build info.
withExeLBI :: PackageDescription -> LocalBuildInfo
           -> (Executable -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withExeLBI pkg lbi f =
    sequence_
        [ f exe clbi
        | (clbi@ExeComponentLocalBuildInfo{}, _) <- componentsConfigs lbi
        , CExe exe <- [getComponent pkg (componentLocalName clbi)] ]

withTestLBI :: PackageDescription -> LocalBuildInfo
            -> (TestSuite -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withTestLBI pkg lbi f =
    sequence_
        [ f test clbi
        | (clbi@TestComponentLocalBuildInfo{}, _) <- componentsConfigs lbi
        , CTest test <- [getComponent pkg (componentLocalName clbi)] ]

testLBIs :: LocalBuildInfo -> [ComponentLocalBuildInfo]
testLBIs lbi = [ clbi | (clbi@TestComponentLocalBuildInfo{}, _) <- componentsConfigs lbi ]

{-# DEPRECATED withComponentsLBI "Use withAllComponentsInBuildOrder" #-}
withComponentsLBI :: PackageDescription -> LocalBuildInfo
                  -> (Component -> ComponentLocalBuildInfo -> IO ())
                  -> IO ()
withComponentsLBI = withAllComponentsInBuildOrder

-- | Perform the action on each buildable 'Library' or 'Executable' (Component)
-- in the PackageDescription, subject to the build order specified by the
-- 'compBuildOrder' field of the given 'LocalBuildInfo'
withAllComponentsInBuildOrder :: PackageDescription -> LocalBuildInfo
                              -> (Component -> ComponentLocalBuildInfo -> IO ())
                              -> IO ()
withAllComponentsInBuildOrder pkg lbi f =
    sequence_
      [ f (getLocalComponent pkg clbi) clbi
      | clbi <- allComponentsInBuildOrder lbi ]

withComponentsInBuildOrder :: PackageDescription -> LocalBuildInfo
                           -> [ComponentName]
                           -> (Component -> ComponentLocalBuildInfo -> IO ())
                           -> IO ()
withComponentsInBuildOrder pkg lbi cnames f =
    sequence_
      [ f (getLocalComponent pkg clbi) clbi
      | clbi <- componentsInBuildOrder lbi cnames ]

allComponentsInBuildOrder :: LocalBuildInfo
                          -> [ComponentLocalBuildInfo]
allComponentsInBuildOrder lbi =
    componentsInBuildOrder' lbi
      [ componentUnitId clbi | (clbi, _) <- componentsConfigs lbi ]

componentsInBuildOrder :: LocalBuildInfo -> [ComponentName]
                       -> [ComponentLocalBuildInfo]
componentsInBuildOrder lbi cnames =
    componentsInBuildOrder' lbi
        (concatMap (componentNameToUnitIds lbi) cnames)

componentsInBuildOrder' :: LocalBuildInfo -> [UnitId]
                       -> [ComponentLocalBuildInfo]
componentsInBuildOrder' lbi cids =
      map ((\(clbi,_,_) -> clbi) . vertexToNode)
    . postOrder graph
    . map (\cid -> fromMaybe (noSuchComp cid) (keyToVertex cid))
    $ cids
  where
    (graph, vertexToNode, keyToVertex) =
      graphFromEdges (map (\(clbi,internal_deps) ->
                            (clbi,componentUnitId clbi,internal_deps)) (componentsConfigs lbi))

    noSuchComp cid = error $ "internal error: componentsInBuildOrder: "
                            ++ "no such component: " ++ show cid

    postOrder :: Graph -> [Vertex] -> [Vertex]
    postOrder g vs = postorderF (dfs g vs) []

    postorderF   :: Forest a -> [a] -> [a]
    postorderF ts = foldr (.) id $ map postorderT ts

    postorderT :: Tree a -> [a] -> [a]
    postorderT (Node a ts) = postorderF ts . (a :)

checkComponentsCyclic :: Ord key => [(node, key, [key])]
                      -> Maybe [(node, key, [key])]
checkComponentsCyclic es =
    let (graph, vertexToNode, _) = graphFromEdges es
        cycles                   = [ flatten c | c <- scc graph, isCycle c ]
        isCycle (Node v [])      = selfCyclic v
        isCycle _                = True
        selfCyclic v             = v `elem` graph ! v
     in case cycles of
         []    -> Nothing
         (c:_) -> Just (map vertexToNode c)

-- | Determine the directories containing the dynamic libraries of the
-- transitive dependencies of the component we are building.
--
-- When wanted, and possible, returns paths relative to the installDirs 'prefix'
depLibraryPaths :: Bool -- ^ Building for inplace?
                -> Bool -- ^ Generate prefix-relative library paths
                -> LocalBuildInfo
                -> ComponentLocalBuildInfo -- ^ Component that is being built
                -> IO [FilePath]
depLibraryPaths inplace relative lbi clbi = do
    let pkgDescr    = localPkgDescr lbi
        installDirs = absoluteComponentInstallDirs pkgDescr lbi (componentUnitId clbi) NoCopyDest
        executable  = case clbi of
                        ExeComponentLocalBuildInfo {} -> True
                        _                             -> False
        relDir | executable = bindir installDirs
               | otherwise  = libdir installDirs

    let -- TODO: this is kind of inefficient
        internalDeps = [ cid
                       | (cid, _) <- componentPackageDeps clbi
                       -- Test that it's internal
                       , (sub_clbi, _) <- componentsConfigs lbi
                       , componentUnitId sub_clbi == cid ]
        internalLibs = [ getLibDir sub_clbi
                       | sub_clbi <- componentsInBuildOrder'
                                        lbi internalDeps ]
        getLibDir sub_clbi
          | inplace    = componentBuildDir lbi sub_clbi
          | otherwise  = libdir (absoluteComponentInstallDirs pkgDescr lbi (componentUnitId sub_clbi) NoCopyDest)

    let ipkgs          = allPackages (installedPkgs lbi)
        allDepLibDirs  = concatMap Installed.libraryDirs ipkgs

        allDepLibDirs' = internalLibs ++ allDepLibDirs
    allDepLibDirsC <- mapM canonicalizePathNoFail allDepLibDirs'

    let p                = prefix installDirs
        prefixRelative l = isJust (stripPrefix p l)
        libPaths
          | relative &&
            prefixRelative relDir = map (\l ->
                                          if prefixRelative l
                                             then shortRelativePath relDir l
                                             else l
                                        ) allDepLibDirsC
          | otherwise             = allDepLibDirsC

    return libPaths
  where
    -- 'canonicalizePath' fails on UNIX when the directory does not exists.
    -- So just don't canonicalize when it doesn't exist.
    canonicalizePathNoFail p = do
      exists <- doesDirectoryExist p
      if exists
         then canonicalizePath p
         else return p


-- -----------------------------------------------------------------------------
-- Wrappers for a couple functions from InstallDirs

-- | Backwards compatibility function which computes the InstallDirs
-- assuming that @$libname@ points to the public library (or some fake
-- package identifier if there is no public library.)  IF AT ALL
-- POSSIBLE, please use 'absoluteComponentInstallDirs' instead.
absoluteInstallDirs :: PackageDescription -> LocalBuildInfo
                    -> CopyDest
                    -> InstallDirs FilePath
absoluteInstallDirs pkg lbi copydest =
    absoluteComponentInstallDirs pkg lbi (localUnitId lbi) copydest

-- | See 'InstallDirs.absoluteInstallDirs'.
absoluteComponentInstallDirs :: PackageDescription -> LocalBuildInfo
                             -> UnitId
                             -> CopyDest
                             -> InstallDirs FilePath
absoluteComponentInstallDirs pkg lbi uid copydest =
  InstallDirs.absoluteInstallDirs
    (packageId pkg)
    uid
    (compilerInfo (compiler lbi))
    copydest
    (hostPlatform lbi)
    (installDirTemplates lbi)

-- | Backwards compatibility function which computes the InstallDirs
-- assuming that @$libname@ points to the public library (or some fake
-- package identifier if there is no public library.)  IF AT ALL
-- POSSIBLE, please use 'prefixRelativeComponentInstallDirs' instead.
prefixRelativeInstallDirs :: PackageId -> LocalBuildInfo
                          -> InstallDirs (Maybe FilePath)
prefixRelativeInstallDirs pkg_descr lbi =
    prefixRelativeComponentInstallDirs pkg_descr lbi (localUnitId lbi)

-- |See 'InstallDirs.prefixRelativeInstallDirs'
prefixRelativeComponentInstallDirs :: PackageId -> LocalBuildInfo
                                   -> UnitId
                                   -> InstallDirs (Maybe FilePath)
prefixRelativeComponentInstallDirs pkg_descr lbi uid =
  InstallDirs.prefixRelativeInstallDirs
    (packageId pkg_descr)
    uid
    (compilerInfo (compiler lbi))
    (hostPlatform lbi)
    (installDirTemplates lbi)

substPathTemplate :: PackageId -> LocalBuildInfo
                  -> UnitId
                  -> PathTemplate -> FilePath
substPathTemplate pkgid lbi uid = fromPathTemplate
                                    . ( InstallDirs.substPathTemplate env )
    where env = initialPathTemplateEnv
                   pkgid
                   uid
                   (compilerInfo (compiler lbi))
                   (hostPlatform lbi)
