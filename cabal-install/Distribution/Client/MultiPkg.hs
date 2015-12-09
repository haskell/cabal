{-# LANGUAGE BangPatterns, RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}

-- | An experimental new UI for cabal for working with multiple packages
-----------------------------------------------------------------------------
module Distribution.Client.MultiPkg (
    -- * High level things
    configure,
    build,
  ) where

import           Distribution.Client.ProjectConfig
import           Distribution.Client.ProjectPlanning
import           Distribution.Client.ProjectBuilding

import           Distribution.Client.Types hiding (BuildResult, BuildSuccess(..), BuildFailure(..), DocsResult(..), TestsResult(..))
import qualified Distribution.Client.InstallPlan as InstallPlan
import           Distribution.Client.Dependency
import qualified Distribution.Client.ComponentDeps as CD
import           Distribution.Client.Targets
import           Distribution.Client.DistDirLayout
import           Distribution.Client.FileStatusCache (FilePathGlob(..), matchFileGlob)
import           Distribution.Client.Glob --TODO [code cleanup] keep globbing in one place, here, not in file status cache
import           Distribution.Client.Config (defaultCabalDir)
import           Distribution.Client.Setup hiding (packageName, cabalVersion)

import           Distribution.Package
import qualified Distribution.PackageDescription as Cabal
import           Distribution.PackageDescription (FlagAssignment)
import qualified Distribution.InstalledPackageInfo as Installed
import qualified Distribution.Simple.PackageIndex as PackageIndex
import qualified Distribution.Client.PackageIndex as SourcePackageIndex
import qualified Distribution.Simple.Setup as Cabal
import qualified Distribution.Simple.BuildTarget as Cabal
import qualified Distribution.Simple.LocalBuildInfo as Cabal

import           Distribution.Simple.Utils hiding (matchFileGlob)
import           Distribution.Verbosity
import           Distribution.Text

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Graph as Graph
import qualified Data.Tree  as Tree

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Monoid

import           System.FilePath
import           System.Directory


------------------------------------------------------------------------------
-- * Understanding this module
------------------------------------------------------------------------------

-- This module deals with building and incrementally rebuilding a collection
-- of packages. It is what backs the "cabal build" and "configure" commands,
-- as well as being a core part of "run", "test", "bench" and others. 
--
-- The primary thing is in fact rebuilding (and trying to make that quick by
-- not redoing unnecessary work), so building from scratch is just a special
-- case.
--
-- The build process and the code can be understood by breaking it down into
-- three major parts:
--
-- * The 'ElaboratedInstallPlan' type
--
-- * The "what to do" phase, where we look at the all input configuration
--   (project files, .cabal files, command line etc) and produce a detailed
--   plan of what to do -- the 'ElaboratedInstallPlan'.
--
-- * The "do it" phase, where we take the 'ElaboratedInstallPlan' and we
-- re-execute it.
--
-- As far as possible, the "what to do" phase embodies all the policy, leaving
-- the "do it" phase policy free. The first phase contains more of the
-- complicated logic, but it is contained in code that is either pure or just
-- has read effects (except cache updates). Then the second phase does all the
-- actions to build packages, but as far as possible it just follows the
-- instructions and avoids any logic for deciding what to do (apart from
-- recompilation avoidance in executing the plan).
--
-- This division helps us keep the code under control, making it easier to
-- understand, test and debug. So when you are extending this module, please
-- think about which parts of your change belong in which part. It is
-- perfectly ok to extend the description of what to do (i.e. the 
-- 'ElaboratedInstallPlan') if that helps keep the policy decisions in the
-- first phase. Also, the second phase does not have direct access to any of
-- the input configuration anyway; all the information has to flow via the
-- 'ElaboratedInstallPlan'.


------------------------------------------------------------------------------
-- * Top level commands: build and configure
------------------------------------------------------------------------------

build :: Verbosity
      -> GlobalFlags
      -> ConfigFlags
      -> ConfigExFlags
      -> InstallFlags
      -> Cabal.HaddockFlags
      -> [String]
      -> IO ()
build verbosity
      globalFlags
      configFlags configExFlags
      installFlags haddockFlags
      targetStrings = do

    cabalDir <- defaultCabalDir
    let cabalDirLayout = defaultCabalDirLayout cabalDir

    projectRootDir <- findProjectRoot
    let distDirLayout = defaultDistDirLayout projectRootDir

    let (cliConfig,
         cliBuildSettings) = convertLegacyCommandLineFlags
                               globalFlags
                               configFlags configExFlags
                               installFlags haddockFlags

    userTargets <- readUserTargets verbosity targetStrings

    -- The (re)build is split into two major parts:
    --  * decide what to do
    --  * do it

    -- Phase 1: decide what to do.
    --
    -- 1.a) Take the project configuration and make a plan for how to build
    -- everything in the project. This is independent of any specific targets
    -- the user has asked for.
    --
    (elaboratedInstallPlan, sharedPackageConfig, projectConfig) <-
      rebuildInstallPlan verbosity
                         projectRootDir distDirLayout cabalDirLayout
                         cliConfig

    --TODO: [nice to have] some debug or status feature to write out the full
    -- elaboratedInstallPlan details
    -- For now can use this:
    writeFile (distProjectCacheFile distDirLayout "plan.txt") $
               unlines $ show sharedPackageConfig
                       : map show (InstallPlan.toList elaboratedInstallPlan)

    let buildSettings = resolveBuildTimeSettings
                          verbosity cabalDirLayout
                          (projectConfigBuildOnly projectConfig)
                          cliBuildSettings

    -- The plan for what to do is represented by an 'ElaboratedInstallPlan'

    -- 1.b) Now given the specific targets the user has asked for, decide
    -- which bits of the plan we will want to execute.
    --
    elaboratedInstallPlan' <-
      selectTargets verbosity
                    cabalDirLayout
                    elaboratedInstallPlan
                    buildSettings
                    userTargets

    -- Phase 2: now do it.
    --
    -- Execute all or parts of the description of what to do to build or
    -- rebuild the various packages needed.
    --
    unless (buildSettingDryRun buildSettings) $
      rebuildTargets verbosity
                     distDirLayout
                     elaboratedInstallPlan'
                     sharedPackageConfig
                     buildSettings

    -- Note that it is a deliberate design choice that the 'userTargets' is
    -- not passed to phase 1, and the various bits of input config is not
    -- passed to phase 2.
    --
    -- We make the install plan without looking at the particular targets the
    -- user asks us to build. The set of available things we can build is
    -- discovered from the env and config and is used to make the install plan.
    -- The targets just tell us which parts of the install plan to execute.
    --
    -- Conversely, executing the plan does not directly depend on any of the
    -- input config. The bits that are needed (or better, the decisions based
    -- on it) all go into the install plan.

    -- Notionally, the 'BuildFlags' should be things that do not affect what
    -- we build, just how we do it. These ones of course do 

-- To a first approximation, configure just runs the first phase of 'build'
-- where we bring the install plan up to date (thus checking that it's
-- possible).
--
-- The only difference is that 'configure' also allows the user to specify
-- some extra local temporary config flags.
--

configure :: Verbosity
          -> GlobalFlags
          -> ConfigFlags
          -> ConfigExFlags
          -> InstallFlags
          -> Cabal.HaddockFlags
          -> [String]
          -> IO ()
configure verbosity
          globalFlags
          configFlags configExFlags
          installFlags haddockFlags
          _extraArgs = do

    -- In this prototype version we don't yet support extra local temporary
    -- config, so it's really just the first phase of build.

    projectRootDir <- findProjectRoot
    cabalDir       <- defaultCabalDir
    let distDirLayout  = defaultDistDirLayout projectRootDir
        cabalDirLayout = defaultCabalDirLayout cabalDir
        (cliConfig,
         _cliBuildSettings) = convertLegacyCommandLineFlags
                                globalFlags
                                configFlags configExFlags
                                installFlags haddockFlags

    _ <- rebuildInstallPlan verbosity
                            projectRootDir distDirLayout cabalDirLayout
                            cliConfig

    return ()


-- | Find the root of this project.
--
-- Searches for an explicit @cabal.project@ file, in the current directory or
-- parent directories. If no project file is found then the current dir is the
-- project root (and the project will use an implicit config).
--
findProjectRoot :: IO FilePath
findProjectRoot = do

    curdir  <- getCurrentDirectory
    homedir <- getHomeDirectory

    -- search upwards if we get to the users home dir or the filesystem root, then use the current dir
    let probe dir | isDrive dir || dir == homedir
                  = return curdir -- implicit project root
        probe dir = do
          exists <- doesFileExist (dir </> "cabal.project")
          if exists
            then return dir       -- explicit project root
            else probe (takeDirectory dir)

    probe curdir
   --TODO: [nice to have] add compat support for old style sandboxes



------------------------------------------------------------------------------
-- * Taking user targets into account, selecting what to build
------------------------------------------------------------------------------


selectTargets :: Verbosity
              -> CabalDirLayout
              -> ElaboratedInstallPlan
              -> BuildTimeSettings
              -> [UserTarget]
              -> IO ElaboratedInstallPlan
selectTargets verbosity
              CabalDirLayout{cabalWorldFile}
              installPlan
              buildSettings
              userTargets = do

    -- First do some defaulting, if no targets are given, use the package
    -- in the current directory if any.
    --
    userTargets' <- case userTargets of
      [] -> do res <- matchFileGlob "." (GlobFile (Glob [WildCard, Literal ".cabal"]))
               case res of
                 [local] -> return [UserTargetLocalCabalFile local]
                 []      -> die $ "TODO: [required feature] decide what to do when no targets are specified and there's no local package, suggest 'all' target?"
                 files   -> die $ "Multiple cabal files found.\n"
                               ++ "Please use only one of: "
                               ++ intercalate ", " files
      _  -> return userTargets

    -- Expand the world target (not really necessary) and disambiguate case
    -- insensitive package names to actual package names.
    --
    packageTargets <- concat <$> mapM (expandUserTarget cabalWorldFile) userTargets'
    let packageSpecifiers :: [PackageSpecifier (PackageLocation ())]
        (problems, packageSpecifiers) =
           disambiguatePackageTargets available availableExtra packageTargets

        --TODO: [required eventually] we're using an empty set of known package names here, which
        -- means we cannot resolve names of packages other than those that are
        -- directly in the current plan. We ought to keep a set of the known
        -- hackage packages so we can resolve names to those. Though we don't
        -- really need that until we can do something sensible with packages
        -- outside of the project.
        available :: SourcePackageIndex.PackageIndex PackageId
        available      = mempty
        availableExtra = map packageName (InstallPlan.toList installPlan)
    reportPackageTargetProblems verbosity problems

    -- Now check if those targets belong to the current project or not.
    -- Ultimately we want to do something sensible for targets not in this
    -- project, but for now we just bail. This gives us back the ipkgid from
    -- the plan.
    --
    targetPkgids <- checkTargets installPlan packageSpecifiers

    -- Finally, prune the install plan to cover just those target packages
    -- and their deps.
    --
    let installPlan' :: ElaboratedInstallPlan
        installPlan' = prunePlanToTargets targetPkgids' installPlan

        targetPkgids' = map (\t -> (t, Nothing)) targetPkgids
        --TODO: [required feature] ^^ add in the individual build targets

    -- Tell the user what we're going to do
    checkPrintPlan verbosity
                   installPlan'
                   buildSettings

    return installPlan'


checkTargets :: ElaboratedInstallPlan
             -> [PackageSpecifier (PackageLocation ())]
             -> IO [InstalledPackageId]
checkTargets installPlan targets = do

    -- Our first task is to work out if a target refers to something inside
    -- the project or outside, since we will behave differently in these cases.
    --
    -- It's not quite as simple as checking if the target refers to a package
    -- name that is inside the project because one might refer to a local
    -- package dir that is not in the project but that has the same name (e.g.
    -- a different local fork of a package). So the logic looks like this:
    -- * for targets with a package location, check it is in the set of
    --   locations of packages within the project
    -- * for targets with just a name, just check it is in the set of names of
    --   packages in the project.

    let projLocalPkgLocs =
          Map.fromList
            [ (fmap (const ()) (pkgSourceLocation pkg), installedPackageId pkg)
            | InstallPlan.Configured pkg <- InstallPlan.toList installPlan ]

        projAllPkgs =
          Map.fromList
            [ (packageName pkg, installedPackageId pkg)
            | pkg <- InstallPlan.toList installPlan ]
        --TODO: [research required] what if the solution has multiple versions of this package?
        --      e.g. due to setup deps or due to multiple independent sets of
        --      packages being built (e.g. ghc + ghcjs in a project)

    mapM (checkTarget projLocalPkgLocs projAllPkgs) targets

  where
    checkTarget projLocalPkgLocs _projAllPkgs (SpecificSourcePackage loc) = do
      absloc <- canonicalizePackageLocation loc
      case Map.lookup absloc projLocalPkgLocs of
        Nothing    -> die $ show loc ++ " is not in the project"
        Just pkgid -> return pkgid

    checkTarget _projLocalPkgLocs projAllPkgs (NamedPackage pkgname _constraints) = do
      case Map.lookup pkgname projAllPkgs of
        Nothing     -> die $ display pkgname ++ " is not in the project"
        Just ipkgid -> return ipkgid

canonicalizePackageLocation :: PackageLocation a -> IO (PackageLocation a)
canonicalizePackageLocation loc =
  case loc of
    LocalUnpackedPackage path -> LocalUnpackedPackage <$> canonicalizePath path
    LocalTarballPackage  path -> LocalTarballPackage  <$> canonicalizePath path
    _                         -> return loc



prunePlanToTargets :: [(InstalledPackageId, Maybe [Cabal.BuildTarget])]
                   -> ElaboratedInstallPlan -> ElaboratedInstallPlan
prunePlanToTargets targets installPlan =
      installPlan'
    where
      Right installPlan' =
        InstallPlan.new False $
          PackageIndex.fromList $
            prunedPkgs

      prunedPkgs = dependencyClosure
                     (CD.flatDeps . depends)
                     (map pruneDependencies (InstallPlan.toList installPlan))
                     (map fst targets)

      buildTargets      = Map.fromList [ (ipkgid, ts)
                                       | (ipkgid, Just ts) <- targets ]
      hasReverseLibDeps = Set.fromList [ depid | pkg <- prunedPkgs
                                       , depid <- CD.flatDeps (depends pkg) ]

      pruneDependencies (InstallPlan.Configured pkg) =
          InstallPlan.Configured pkg {
            pkgDependencies = CD.filterDeps keepNeeded (pkgDependencies pkg),
            pkgBuildTargets =
              if Set.member ipkgid hasReverseLibDeps
                then Nothing  -- no specific targets, i.e. build everything
                else Map.lookup ipkgid buildTargets
                              -- any specific targets, or everything
          }
        where
          keepNeeded (CD.ComponentTest  _) _ = keepTestsuites
          keepNeeded (CD.ComponentBench _) _ = keepBenchmarks
          keepNeeded _                     _ = True

          ipkgid         = installedPackageId pkg
          bts            = fromMaybe [] (Map.lookup ipkgid buildTargets)
          keepTestsuites =
            not $ null [ () | Cabal.CTestName _ <- map Cabal.buildTargetComponentName bts ]
          keepBenchmarks =
            not $ null [ () | Cabal.CBenchName _ <- map Cabal.buildTargetComponentName bts ]

      pruneDependencies pkg = pkg


dependencyClosure :: HasInstalledPackageId pkg
                  => (pkg -> [InstalledPackageId])
                  -> [pkg]
                  -> [InstalledPackageId]
                  -> [pkg]
dependencyClosure deps allpkgs =
    map vertexToPkg
  . concatMap Tree.flatten
  . Graph.dfs graph
  . map pkgidToVertex
  where
    (graph, vertexToPkg, pkgidToVertex) = dependencyGraph deps allpkgs

dependencyGraph :: HasInstalledPackageId pkg
                => (pkg -> [InstalledPackageId])
                -> [pkg]
                -> (Graph.Graph,
                    Graph.Vertex -> pkg,
                    InstalledPackageId -> Graph.Vertex)
dependencyGraph deps pkgs =
    (graph, vertexToPkg', pkgidToVertex')
  where
    (graph, vertexToPkg, pkgidToVertex) =
      Graph.graphFromEdges [ ( pkg, installedPackageId pkg, deps pkg ) 
                           | pkg <- pkgs ]
    vertexToPkg'   = (\(pkg,_,_) -> pkg)
                   . vertexToPkg
    pkgidToVertex' = fromMaybe (error "dependencyGraph: lookup failure")
                   . pkgidToVertex


-------------------------------
-- Displaying plan info
--

checkPrintPlan :: Verbosity
               -> ElaboratedInstallPlan
               -> BuildTimeSettings
               -> IO ()
checkPrintPlan verbosity installPlan buildSettings =
    printPlan verbosity dryRun (linearizeInstallPlan installPlan)
    --TODO: [required eventually] see Install.hs for other things checkPrintPlan ought to do too
  where
    dryRun = buildSettingDryRun buildSettings


linearizeInstallPlan :: ElaboratedInstallPlan -> [ElaboratedReadyPackage]
linearizeInstallPlan =
    unfoldr next
  where
    next plan = case InstallPlan.ready plan of
      []          -> Nothing
      ((pkg,_):_) -> Just (pkg, plan')
        where
          pkgid  = installedPackageId pkg
          ipkg   = Installed.emptyInstalledPackageInfo {
                     Installed.sourcePackageId    = packageId pkg,
                     Installed.installedPackageId = pkgid
                   }
          plan'  = InstallPlan.completed pkgid (Just ipkg)
                     (BuildOk True DocsNotTried TestsNotTried)
                     (InstallPlan.processing [pkg] plan)
    --TODO: [code cleanup] This is a bit of a hack, pretending that each package is installed
    -- could we use InstallPlan.topologicalOrder?

printPlan :: Verbosity
          -> Bool -- is dry run
          -> [ElaboratedReadyPackage]
          -> IO ()
printPlan _         _      []   = return ()
printPlan verbosity dryRun pkgs
  | verbosity >= verbose
  = notice verbosity $ unlines $
      ("In order, the following " ++ wouldWill ++ " be built:")
    : map showPkgAndReason pkgs

  | otherwise
  = notice verbosity $ unlines $
      ("In order, the following " ++ wouldWill
       ++ " be built (use -v for more details):")
    : map showPkg pkgs
  where
    wouldWill | dryRun    = "would"
              | otherwise = "will"

    showPkg pkg = display (packageId pkg)

    showPkgAndReason :: ElaboratedReadyPackage -> String
    showPkgAndReason (ReadyPackage pkg' _) =
      display (packageId pkg') ++
      showFlagAssignment (nonDefaultFlags pkg') ++
      showStanzas pkg'

    nonDefaultFlags :: ElaboratedConfiguredPackage -> FlagAssignment
    nonDefaultFlags pkg = pkgFlagAssignment pkg \\ pkgFlagDefaults pkg

    showStanzas pkg = concat $ [ " *test"  | pkgTestsuitesEnable pkg ]
                            ++ [ " *bench" | pkgBenchmarksEnable pkg ]

    -- TODO: [code cleanup] this should be a proper function in a proper place
    showFlagAssignment :: FlagAssignment -> String
    showFlagAssignment = concatMap ((' ' :) . showFlagValue)
    showFlagValue (f, True)   = '+' : showFlagName f
    showFlagValue (f, False)  = '-' : showFlagName f
    showFlagName (Cabal.FlagName f) = f

