{-# LANGUAGE DeriveGeneric #-}

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
        inplacePackageId,

        -- * Buildable package components
        Component(..),
        ComponentName(..),
        showComponentName,
        ComponentLocalBuildInfo(..),
        LibraryName(..),
        foldComponent,
        componentName,
        componentBuildInfo,
        componentEnabled,
        componentDisabledReason,
        ComponentDisabledReason(..),
        pkgComponents,
        pkgEnabledComponents,
        lookupComponent,
        getComponent,
        getComponentLocalBuildInfo,
        allComponentsInBuildOrder,
        componentsInBuildOrder,
        checkComponentsCyclic,

        withAllComponentsInBuildOrder,
        withComponentsInBuildOrder,
        withComponentsLBI,
        withLibLBI,
        withExeLBI,
        withTestLBI,

        -- * Installation directories
        module Distribution.Simple.InstallDirs,
        absoluteInstallDirs, prefixRelativeInstallDirs,
        substPathTemplate
  ) where


import Distribution.Simple.InstallDirs hiding (absoluteInstallDirs,
                                               prefixRelativeInstallDirs,
                                               substPathTemplate, )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.PackageDescription
         ( PackageDescription(..), withLib, Library(libBuildInfo), withExe
         , Executable(exeName, buildInfo), withTest, TestSuite(..)
         , BuildInfo(buildable), Benchmark(..), ModuleRenaming(..) )
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.Package
         ( PackageId, Package(..), InstalledPackageId(..), PackageKey
         , PackageName )
import Distribution.Simple.Compiler
         ( Compiler, compilerInfo, PackageDBStack, OptimisationLevel )
import Distribution.Simple.PackageIndex
         ( InstalledPackageIndex )
import Distribution.ModuleName ( ModuleName )
import Distribution.Simple.Setup
         ( ConfigFlags )
import Distribution.Text
         ( display )
import Distribution.System
          ( Platform )

import Data.Array ((!))
import Data.Binary (Binary)
import Data.Graph
import Data.List (nub, find)
import Data.Maybe
import Data.Tree  (flatten)
import GHC.Generics (Generic)
import Data.Map (Map)

-- | Data cached after configuration step.  See also
-- 'Distribution.Simple.Setup.ConfigFlags'.
data LocalBuildInfo = LocalBuildInfo {
        configFlags   :: ConfigFlags,
        -- ^ Options passed to the configuration step.
        -- Needed to re-run configuration when .cabal is out of date
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
        componentsConfigs   :: [(ComponentName, ComponentLocalBuildInfo, [ComponentName])],
                -- ^ All the components to build, ordered by topological sort, and with their dependencies
                -- over the intrapackage dependency graph
        installedPkgs :: InstalledPackageIndex,
                -- ^ All the info about the installed packages that the
                -- current package depends on (directly or indirectly).
        pkgDescrFile  :: Maybe FilePath,
                -- ^ the filename containing the .cabal file, if available
        localPkgDescr :: PackageDescription,
                -- ^ The resolved package description, that does not contain
                -- any conditionals.
        pkgKey        :: PackageKey,
                -- ^ The package key for the current build, calculated from
                -- the package ID and the dependency graph.
        instantiatedWith :: [(ModuleName, (InstalledPackageInfo, ModuleName))],
        withPrograms  :: ProgramConfiguration, -- ^Location and args for all programs
        withPackageDB :: PackageDBStack,  -- ^What package database to use, global\/user
        withVanillaLib:: Bool,  -- ^Whether to build normal libs.
        withProfLib   :: Bool,  -- ^Whether to build profiling versions of libs.
        withSharedLib :: Bool,  -- ^Whether to build shared versions of libs.
        withDynExe    :: Bool,  -- ^Whether to link executables dynamically
        withProfExe   :: Bool,  -- ^Whether to build executables for profiling.
        withOptimization :: OptimisationLevel, -- ^Whether to build with optimization (if available).
        withGHCiLib   :: Bool,  -- ^Whether to build libs suitable for use with GHCi.
        splitObjs     :: Bool,  -- ^Use -split-objs with GHC, if available
        stripExes     :: Bool,  -- ^Whether to strip executables during install
        stripLibs     :: Bool,  -- ^Whether to strip libraries during install
        progPrefix    :: PathTemplate, -- ^Prefix to be prepended to installed executables
        progSuffix    :: PathTemplate -- ^Suffix to be appended to installed executables
  } deriving (Generic, Read, Show)

instance Binary LocalBuildInfo

-- | External package dependencies for the package as a whole. This is the
-- union of the individual 'componentPackageDeps', less any internal deps.
externalPackageDeps :: LocalBuildInfo -> [(InstalledPackageId, PackageId)]
externalPackageDeps lbi =
    -- TODO:  what about non-buildable components?
    nub [ (ipkgid, pkgid)
        | (_,clbi,_)      <- componentsConfigs lbi
        , (ipkgid, pkgid) <- componentPackageDeps clbi
        , not (internal pkgid) ]
  where
    -- True if this dependency is an internal one (depends on the library
    -- defined in the same package).
    internal pkgid = pkgid == packageId (localPkgDescr lbi)

-- | The installed package Id we use for local packages registered in the local
-- package db. This is what is used for intra-package deps between components.
--
inplacePackageId :: PackageId -> InstalledPackageId
inplacePackageId pkgid = InstalledPackageId (display pkgid ++ "-inplace")

-- -----------------------------------------------------------------------------
-- Buildable components

data Component = CLib   Library
               | CExe   Executable
               | CTest  TestSuite
               | CBench Benchmark
               deriving (Show, Eq, Read)

data ComponentName = CLibName   -- currently only a single lib
                   | CExeName   String
                   | CTestName  String
                   | CBenchName String
                   deriving (Eq, Generic, Ord, Read, Show)

instance Binary ComponentName

showComponentName :: ComponentName -> String
showComponentName CLibName          = "library"
showComponentName (CExeName   name) = "executable '" ++ name ++ "'"
showComponentName (CTestName  name) = "test suite '" ++ name ++ "'"
showComponentName (CBenchName name) = "benchmark '" ++ name ++ "'"

data ComponentLocalBuildInfo
  = LibComponentLocalBuildInfo {
    -- | Resolved internal and external package dependencies for this component.
    -- The 'BuildInfo' specifies a set of build dependencies that must be
    -- satisfied in terms of version ranges. This field fixes those dependencies
    -- to the specific versions available on this machine for this compiler.
    componentPackageDeps :: [(InstalledPackageId, PackageId)],
    componentExposedModules :: [Installed.ExposedModule],
    componentPackageRenaming :: Map PackageName ModuleRenaming,
    componentLibraries :: [LibraryName]
  }
  | ExeComponentLocalBuildInfo {
    componentPackageDeps :: [(InstalledPackageId, PackageId)],
    componentPackageRenaming :: Map PackageName ModuleRenaming
  }
  | TestComponentLocalBuildInfo {
    componentPackageDeps :: [(InstalledPackageId, PackageId)],
    componentPackageRenaming :: Map PackageName ModuleRenaming
  }
  | BenchComponentLocalBuildInfo {
    componentPackageDeps :: [(InstalledPackageId, PackageId)],
    componentPackageRenaming :: Map PackageName ModuleRenaming
  }
  deriving (Generic, Read, Show)

instance Binary ComponentLocalBuildInfo

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

data LibraryName = LibraryName String
    deriving (Generic, Read, Show)

instance Binary LibraryName

componentBuildInfo :: Component -> BuildInfo
componentBuildInfo =
  foldComponent libBuildInfo buildInfo testBuildInfo benchmarkBuildInfo

componentName :: Component -> ComponentName
componentName =
  foldComponent (const CLibName)
                (CExeName . exeName)
                (CTestName . testName)
                (CBenchName . benchmarkName)

-- | All the components in the package (libs, exes, or test suites).
--
pkgComponents :: PackageDescription -> [Component]
pkgComponents pkg =
    [ CLib  lib | Just lib <- [library pkg] ]
 ++ [ CExe  exe | exe <- executables pkg ]
 ++ [ CTest tst | tst <- testSuites  pkg ]
 ++ [ CBench bm | bm  <- benchmarks  pkg ]

-- | All the components in the package that are buildable and enabled.
-- Thus this excludes non-buildable components and test suites or benchmarks
-- that have been disabled.
--
pkgEnabledComponents :: PackageDescription -> [Component]
pkgEnabledComponents = filter componentEnabled . pkgComponents

componentEnabled :: Component -> Bool
componentEnabled = isNothing . componentDisabledReason

data ComponentDisabledReason = DisabledComponent
                             | DisabledAllTests
                             | DisabledAllBenchmarks

componentDisabledReason :: Component -> Maybe ComponentDisabledReason
componentDisabledReason (CLib  lib)
  | not (buildable (libBuildInfo lib))      = Just DisabledComponent
componentDisabledReason (CExe  exe)
  | not (buildable (buildInfo exe))         = Just DisabledComponent
componentDisabledReason (CTest tst)
  | not (buildable (testBuildInfo tst))     = Just DisabledComponent
  | not (testEnabled tst)                   = Just DisabledAllTests
componentDisabledReason (CBench bm)
  | not (buildable (benchmarkBuildInfo bm)) = Just DisabledComponent
  | not (benchmarkEnabled bm)               = Just DisabledAllBenchmarks
componentDisabledReason _                   = Nothing

lookupComponent :: PackageDescription -> ComponentName -> Maybe Component
lookupComponent pkg CLibName =
    fmap CLib $ library pkg
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


getComponentLocalBuildInfo :: LocalBuildInfo -> ComponentName -> ComponentLocalBuildInfo
getComponentLocalBuildInfo lbi cname =
    case [ clbi
         | (cname', clbi, _) <- componentsConfigs lbi
         , cname == cname' ] of
      [clbi] -> clbi
      _      -> missingComponent
  where
    missingComponent =
      error $ "internal error: there is no configuration data "
           ++ "for component " ++ show cname


-- |If the package description has a library section, call the given
--  function with the library build info as argument.  Extended version of
-- 'withLib' that also gives corresponding build info.
withLibLBI :: PackageDescription -> LocalBuildInfo
           -> (Library -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withLibLBI pkg_descr lbi f =
    withLib pkg_descr $ \lib ->
      f lib (getComponentLocalBuildInfo lbi CLibName)

-- | Perform the action on each buildable 'Executable' in the package
-- description.  Extended version of 'withExe' that also gives corresponding
-- build info.
withExeLBI :: PackageDescription -> LocalBuildInfo
           -> (Executable -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withExeLBI pkg_descr lbi f =
    withExe pkg_descr $ \exe ->
      f exe (getComponentLocalBuildInfo lbi (CExeName (exeName exe)))

withTestLBI :: PackageDescription -> LocalBuildInfo
            -> (TestSuite -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withTestLBI pkg_descr lbi f =
    withTest pkg_descr $ \test ->
      f test (getComponentLocalBuildInfo lbi (CTestName (testName test)))

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
      [ f (getComponent pkg cname) clbi
      | (cname, clbi) <- allComponentsInBuildOrder lbi ]

withComponentsInBuildOrder :: PackageDescription -> LocalBuildInfo
                                  -> [ComponentName]
                                  -> (Component -> ComponentLocalBuildInfo -> IO ())
                                  -> IO ()
withComponentsInBuildOrder pkg lbi cnames f =
    sequence_
      [ f (getComponent pkg cname') clbi
      | (cname', clbi) <- componentsInBuildOrder lbi cnames ]

allComponentsInBuildOrder :: LocalBuildInfo
                          -> [(ComponentName, ComponentLocalBuildInfo)]
allComponentsInBuildOrder lbi =
    componentsInBuildOrder lbi
      [ cname | (cname, _, _) <- componentsConfigs lbi ]

componentsInBuildOrder :: LocalBuildInfo -> [ComponentName]
                       -> [(ComponentName, ComponentLocalBuildInfo)]
componentsInBuildOrder lbi cnames =
      map ((\(clbi,cname,_) -> (cname,clbi)) . vertexToNode)
    . postOrder graph
    . map (\cname -> fromMaybe (noSuchComp cname) (keyToVertex cname))
    $ cnames
  where
    (graph, vertexToNode, keyToVertex) =
      graphFromEdges (map (\(a,b,c) -> (b,a,c)) (componentsConfigs lbi))

    noSuchComp cname = error $ "internal error: componentsInBuildOrder: "
                            ++ "no such component: " ++ show cname

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


-- -----------------------------------------------------------------------------
-- Wrappers for a couple functions from InstallDirs

-- |See 'InstallDirs.absoluteInstallDirs'
absoluteInstallDirs :: PackageDescription -> LocalBuildInfo -> CopyDest
                    -> InstallDirs FilePath
absoluteInstallDirs pkg lbi copydest =
  InstallDirs.absoluteInstallDirs
    (packageId pkg)
    (pkgKey lbi)
    (compilerInfo (compiler lbi))
    copydest
    (hostPlatform lbi)
    (installDirTemplates lbi)

-- |See 'InstallDirs.prefixRelativeInstallDirs'
prefixRelativeInstallDirs :: PackageId -> LocalBuildInfo
                          -> InstallDirs (Maybe FilePath)
prefixRelativeInstallDirs pkg_descr lbi =
  InstallDirs.prefixRelativeInstallDirs
    (packageId pkg_descr)
    (pkgKey lbi)
    (compilerInfo (compiler lbi))
    (hostPlatform lbi)
    (installDirTemplates lbi)

substPathTemplate :: PackageId -> LocalBuildInfo
                  -> PathTemplate -> FilePath
substPathTemplate pkgid lbi = fromPathTemplate
                                . ( InstallDirs.substPathTemplate env )
    where env = initialPathTemplateEnv
                   pkgid
                   (pkgKey lbi)
                   (compilerInfo (compiler lbi))
                   (hostPlatform lbi)
