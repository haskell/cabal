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
        withBenchLBI,
        withTestLBI,
        enabledTestLBIs,
        enabledBenchLBIs,
        enabledComponents,

        -- TODO: Don't export me
        ComponentEnabledSpec(..),
        defaultComponentEnabled,
        componentEnabled,
        componentDisabledReason,
        componentNameEnabled,
        componentNameDisabledReason,
        ComponentDisabledReason(..),

        -- * Installation directories
        module Distribution.Simple.InstallDirs,
        absoluteInstallDirs, prefixRelativeInstallDirs,
        absoluteComponentInstallDirs, prefixRelativeComponentInstallDirs,
        substPathTemplate
  ) where

import Distribution.Types.Component
import Distribution.Types.ComponentName
import Distribution.Types.ComponentEnabledSpec
import Distribution.Types.PackageDescription
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.LocalBuildInfo

import Distribution.Simple.InstallDirs hiding (absoluteInstallDirs,
                                               prefixRelativeInstallDirs,
                                               substPathTemplate, )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.PackageDescription
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.Package
import Distribution.Simple.Compiler
import Distribution.Simple.PackageIndex
import Distribution.Simple.Utils

import Data.Array ((!))
import Data.Graph
import Data.List (stripPrefix)
import Data.Maybe
import Data.Tree  (flatten)
import System.FilePath

import System.Directory (doesDirectoryExist, canonicalizePath)

-- -----------------------------------------------------------------------------
-- Configuration information of buildable components

getLocalComponent :: PackageDescription -> ComponentLocalBuildInfo -> Component
getLocalComponent pkg_descr clbi = getComponent pkg_descr (componentLocalName clbi)

componentBuildDir :: LocalBuildInfo -> ComponentLocalBuildInfo -> FilePath
-- For now, we assume that libraries/executables/test-suites/benchmarks
-- are only ever built once.  With Backpack, we need a special case for
-- libraries so that we can handle building them multiple times.
componentBuildDir lbi clbi
    = buildDir lbi </> case componentLocalName clbi of
                        CLibName     -> ""
                        CSubLibName s -> s
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

-- | Perform the action on each enabled 'library' in the package
-- description with the 'ComponentLocalBuildInfo'.
withLibLBI :: PackageDescription -> LocalBuildInfo
           -> (Library -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withLibLBI pkg lbi f =
    sequence_
        [ f lib clbi
        | (clbi@LibComponentLocalBuildInfo{}, _) <- componentsConfigs lbi
        , CLib lib <- [getComponent pkg (componentLocalName clbi)] ]

-- | Perform the action on each enabled 'Executable' in the package
-- description.  Extended version of 'withExe' that also gives corresponding
-- build info.
withExeLBI :: PackageDescription -> LocalBuildInfo
           -> (Executable -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withExeLBI pkg lbi f =
    sequence_
        [ f exe clbi
        | (clbi@ExeComponentLocalBuildInfo{}, _) <- componentsConfigs lbi
        , CExe exe <- [getComponent pkg (componentLocalName clbi)] ]

-- | Perform the action on each enabled 'Benchmark' in the package
-- description.
withBenchLBI :: PackageDescription -> LocalBuildInfo
            -> (Benchmark -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withBenchLBI pkg lbi f =
    sequence_ [ f test clbi | (test, clbi) <- enabledBenchLBIs pkg lbi ]

withTestLBI :: PackageDescription -> LocalBuildInfo
            -> (TestSuite -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withTestLBI pkg lbi f =
    sequence_ [ f test clbi | (test, clbi) <- enabledTestLBIs pkg lbi ]

enabledTestLBIs :: PackageDescription -> LocalBuildInfo
             -> [(TestSuite, ComponentLocalBuildInfo)]
enabledTestLBIs pkg lbi =
        [ (test, clbi)
        | (clbi@TestComponentLocalBuildInfo{}, _) <- componentsConfigs lbi
        , CTest test <- [getComponent pkg (componentLocalName clbi)] ]

enabledBenchLBIs :: PackageDescription -> LocalBuildInfo
             -> [(Benchmark, ComponentLocalBuildInfo)]
enabledBenchLBIs pkg lbi =
        [ (test, clbi)
        | (clbi@BenchComponentLocalBuildInfo{}, _) <- componentsConfigs lbi
        , CBench test <- [getComponent pkg (componentLocalName clbi)] ]

-- | Get a list of all enabled 'Component's (both buildable and
-- requested by the user at configure-time).
--
-- @since 2.0.0.0
enabledComponents :: PackageDescription -> LocalBuildInfo
                  -> [Component]
enabledComponents pkg lbi =
        [ getComponent pkg (componentLocalName clbi)
        | (clbi, _) <- componentsConfigs lbi ]


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
