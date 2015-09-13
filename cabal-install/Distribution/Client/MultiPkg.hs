{-# LANGUAGE BangPatterns, DeriveGeneric,
             RecordWildCards, NamedFieldPuns,
             ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Install
-- Copyright   :  (c) 2005 David Himmelstrup
--                    2007 Bjorn Bringert
--                    2007-2010 Duncan Coutts
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An experimental new UI for cabal for working with multiple packages
-----------------------------------------------------------------------------
module Distribution.Client.MultiPkg (
    -- * High level things
    configure,
    build,

    testBuild,
    buildTargets
  ) where

import           Distribution.Client.Types
import           Distribution.Client.InstallPlan
                   ( GenericInstallPlan, InstallPlan )
import qualified Distribution.Client.InstallPlan as InstallPlan
import           Distribution.Client.Dependency
import           Distribution.Client.Dependency.Types
import qualified Distribution.Client.ComponentDeps as ComponentDeps
import           Distribution.Client.IndexUtils
import           Distribution.Client.Targets
import           Distribution.Client.DistDirLayout
import           Distribution.Client.FileStatusCache
import           Distribution.Client.SetupWrapper
import           Distribution.Client.JobControl
import           Distribution.Client.HttpUtils
import           Distribution.Client.FetchUtils
import qualified Distribution.Client.Tar as Tar
import           Distribution.Client.Config (SavedConfig(..), defaultRemoteRepo, defaultCabalDir, defaultCacheDir)
import           Distribution.Client.Setup hiding (packageName)

import           Distribution.Client.Sandbox
import           Distribution.Client.Sandbox.Types
import           Distribution.Client.Sandbox.Index as Index

--import           Distribution.Client.Utils

import           Distribution.Package
import           Distribution.System
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Configuration as Cabal
import           Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as Installed
import           Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import qualified Distribution.Client.PackageIndex as Client.PackageIndex
import           Distribution.Simple.Compiler
import           Distribution.Simple.Program
import           Distribution.Simple.Program.Db
import qualified Distribution.Simple.GHC as GHC
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import qualified Distribution.Simple.Setup as Cabal
import           Distribution.Simple.Setup (toFlag)
import           Distribution.Simple.Configure (configCompilerEx, configCompilerAuxEx)
import qualified Distribution.Simple.InstallDirs as InstallDirs
--import           Distribution.Client.Setup ()
import           Distribution.Simple.Utils
import           Distribution.Version
import           Distribution.Verbosity
import           Distribution.Text
import           Distribution.ParseUtils ( showPWarning )

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS.Char8
import qualified Data.Digest.Pure.SHA as SHA

import           Control.Monad
import           Control.Exception
import           Control.Concurrent.Async
import           Data.List
import           Data.Either
import           Data.Maybe

import           Data.Binary
import           GHC.Generics (Generic)

import           System.FilePath
import           System.IO
import           System.Directory

import Data.Time
import Debug.Trace


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
      -> UseSandbox
      -> BuildFlags
      -> BuildExFlags
      -> [String]
      -> IO ()
build verbosity
      globalFlags
      configFlags configExFlags useSandbox
      buildFlags _buildExFlags --TODO: do something with these
      userTargets =

    -- The build, or really rebuild, is split into two major parts:

    -- Phase 1: decide what to do.
    --
    -- Take all input configuration and make a description of what to do.
    --
    rebuildInstallPlan verbosity
                       -- all the config:
                       globalFlags
                       configFlags configExFlags
                       useSandbox

    -- The description of what to do is represented by an
    -- 'ElaboratedInstallPlan' (plus an 'ElaboratedSharedConfig' for those
    -- bits that are common between all packages.
    --
    >>= \ (elaboratedInstallPlan, sharedPackageConfig) ->

    -- Phase 2: now do it.
    --
    -- Execute all or parts of the description of what to do to build or
    -- rebuild the various packages needed.
    --
    rebuildTargets verbosity
                   elaboratedInstallPlan
                   sharedPackageConfig
                   userTargets
                   buildFlags

    --TODO: may want to do some earlier parsing/validation of the userTargets
    -- though fully resolving what they refer to requires info from the env
    -- or config (e.g. knowing which packages are in the plan, where they live)
    
    --TODO: we may need to set up the http transport in either or both phases
    -- but we'd really like to do so lazily, but would also like to share if
    -- possible.

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
    -- TODO: The current 'BuildFlags' is a mix of config and build-only, due
    -- to the old command line interface and needs to be cleaned up.


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
          -> UseSandbox
          -> [String]
          -> IO ()
configure verbosity
          globalFlags configFlags configExFlags useSandbox
          _extraArgs = do

    -- In this prototype version we don't yet support extra local temporary
    -- config, so it's really just the first phase of build.

    _ <- rebuildInstallPlan verbosity
                            -- all the config:
                            globalFlags
                            configFlags configExFlags
                            useSandbox

    return ()


------------------------------------------------------------------------------
-- * Elaborated install plan
------------------------------------------------------------------------------

-- "Elaborated" -- worked out with great care and nicety of detail;
--                 executed with great minuteness: elaborate preparations;
--                 elaborate care.
--
-- So here's the idea:
--
-- Rather than a miscellaneous collection of 'ConfigFlags', 'InstallFlags' etc
-- all passed in as separate args and which are then further selected,
-- transformed etc during the execution of the build. Instead we construct
-- an elaborated install plan that includes everything we will need, and then
-- during the execution of the plan we do as little transformation of this
-- info as possible.
--
-- So we're trying to split the work into two phases: construction of the
-- elaborated install plan (which as far as possible should be pure) and
-- then simple execution of that plan without any smarts, just doing what the
-- plan says to do.
--
-- So that means we need a representation of this fully elaborated install
-- plan. The representation consists of two parts:
--
-- * A 'ElaboratedInstallPlan'. This is a 'GenericInstallPlan' with a
--   representation of source packages that includes a lot more detail about
--   that package's individual configuration
--
-- * A 'ElaboratedSharedConfig'. Some package configuration is the same for
--   every package in a plan. Rather than duplicate that info every entry in
--   the 'GenericInstallPlan' we keep that separately.
--
-- The division between the shared and per-package config is /not set in stone
-- for all time/. For example if we wanted to generalise the install plan to
-- describe a situation where we want to build some packages with GHC and some
-- with GHCJS then the platform and compiler would no longer be shared between
-- all packages but would have to be per-package (probably with some sanity
-- condition on the graph structure).
--

-- | The combination of an elaborated install plan plus a
-- 'ElaboratedSharedConfig' contains all the details necessary to be able
-- to execute the plan without having to make further policy decisions.
--
-- It does not include dynamic elements such as resources (such as http
-- connections).
--
type ElaboratedInstallPlan
   = GenericInstallPlan InstalledPackageInfo
                        ElaboratedConfiguredPackage
                        BuildSuccess BuildFailure

data ElaboratedSharedConfig
   = ElaboratedSharedConfig {

       pkgConfigPlatform         :: Platform,
       pkgConfigCompiler         :: Compiler, --TODO: replace with CompilerInfo
       pkgConfigProgramDb        :: ProgramDb, --TODO: no Eq instance

       pkgConfigLibraryProfiling :: Bool
       --TODO: loads more, see the ConfigFlags

     }
  deriving (Show, Generic)

instance Binary ElaboratedSharedConfig

       -- ambient package db stack to use for Setup.hs
       -- package db stack to use for compiling against
       -- package db stack to use for registering into
       -- the compiler
       -- the platform
       -- the program db

data ElaboratedConfiguredPackage
   = ElaboratedConfiguredPackage {

       -- | TODO: we don't need this, just a few bits from it:
       --   build type, spec version
       pkgDescription :: Cabal.GenericPackageDescription,

       pkgSrcId :: PackageId,

       -- | A total flag assignment for the package
       pkgFlagAssignment   :: Cabal.FlagAssignment,

       -- | Which optional stanzas are enabled (testsuites, benchmarks)
       pkgEnabledStanzas   :: [OptionalStanza],

       -- | The exact dependencies (to other plan packages)
       --
       pkgDependencies     :: ComponentDeps.ComponentDeps [InstalledPackageId],

       -- | Where the package comes from, e.g. tarball, local dir etc. This
       --   is not the same as where it may be unpacked to for the build.
       pkgSourceLocation :: PackageLocation (Maybe FilePath),

       --pkgSourceDir ? -- or derived based on package location?
       --pkgBuildDir  ?
       
       pkgBuildStyle             :: BuildStyle,

       pkgSetupPackageDBStack    :: PackageDBStack,
       pkgBuildPackageDBStack    :: PackageDBStack,
       pkgRegisterPackageDBStack :: PackageDBStack,

       -- | The package contains a library and so must be registered
       pkgRequiresRegistration :: Bool,

       pkgDescriptionOverride  :: Maybe LBS.Char8.ByteString

{-
       -- Setup.hs related things:

       -- | The version of the Cabal spec that this package claims to support.
       pkgCabalSpecVersion    :: Version,
 
       -- | The .cabal file build type 
       pkgSetupBuildType      :: Cabal.BuildType,

       -- | The version of the Cabal command line interface that this package
       -- is using. In practice this means the version of the Cabal lib it
       -- was built against (or if none or unknown the minimum spec version).
       pkgSetupHsCabalVersion :: Version
-}

       --TODO: loads more, see the ConfigFlags and hash info below

--     pkgHashCompilerId              :: CompilerId,
--     pkgHashPlatform                :: Platform,
--     pkgHashFlagAssignment          :: FlagAssignment, -- complete not partial
--     pkgHashConfigureOptions        :: [String], -- just ./configure for build-type Configure
--     pkgHashOptimisationLevel       :: OptimisationLevel,
--     pkgHashLibraryShared           :: Bool,
--     pkgHashLibraryProfiling        :: Bool
--     pkgHashLibraryProfilingLevel   :: ProfDetailLevel,
--     pkgHashHpcEnabled              :: Bool,
--     pkgHashToolsVersions     ?
--     pkgHashToolsExtraOptions ?
     }
  deriving (Eq, Show, Generic)

instance Binary ElaboratedConfiguredPackage

instance Package ElaboratedConfiguredPackage where
  packageId = pkgSrcId

instance HasInstalledPackageId ElaboratedConfiguredPackage where
  installedPackageId = fakeInstalledPackageId . packageId

instance PackageFixedDeps ElaboratedConfiguredPackage where
  depends = pkgDependencies

-- | This is used in the install plan to indicate how the package will be
-- built.
--
data BuildStyle =
    -- | The classic approach where the package is built, then the files
    -- installed into some location and the result registered in a package db.
    --
    -- If the package came from a tarball then it's built in a temp dir and
    -- the results discarded.
    BuildAndInstall

    -- | The package is built, but the files are not installed anywhere,
    -- rather the build dir is kept and the package is registered inplace.
    --
    -- Such packages can still subsequently be installed.
    --
    -- Typically 'BuildAndInstall' packages will only depend on other
    -- 'BuildAndInstall' style packages and not on 'BuildInplaceOnly' ones.
    --
  | BuildInplaceOnly
  deriving (Eq, Show, Generic)

instance Binary BuildStyle

type ElaboratedReadyPackage = GenericReadyPackage ElaboratedConfiguredPackage
                                                  InstalledPackageInfo

------------------------------------------------------------------------------
-- * Deciding what to do: making an 'ElaboratedInstallPlan'
------------------------------------------------------------------------------

rebuildInstallPlan :: Verbosity ->
                      GlobalFlags ->
                      ConfigFlags -> ConfigExFlags ->
                      UseSandbox ->
                      IO (ElaboratedInstallPlan, ElaboratedSharedConfig)
rebuildInstallPlan verbosity
                   -- all the config:
                   globalFlags
                   configFlags configExFlags
                   useSandbox =
    return (undefined, undefined)


------------------------------------------------------------------------------
-- * Doing it: executing an 'ElaboratedInstallPlan'
------------------------------------------------------------------------------


rebuildTargets :: Verbosity ->
                  ElaboratedInstallPlan ->
                  ElaboratedSharedConfig ->
                  [String] ->  --TODO: probably want to parse earlier
                  BuildFlags -> --TODO: prune to build-only flags
                  IO ()
rebuildTargets verbosity
               elaboratedInstallPlan
               sharedPackageConfig
               userTargets
               BuildFlags{buildNumJobs} =


    putStrLn $ printPlan elaboratedInstallPlan'
--  mapM_ print [ pkg | InstallPlan.Configured pkg <- InstallPlan.toList elaboratedInstallPlan' ]

    residualPlan <- buildTargets
                      verbosity
                      numJobs
                      transport
                      sharedPackageConfig
                      elaboratedInstallPlan'

    --TODO: this result plan should be saved, resetting all failed and
    -- BuildInplaceOnly packages to original Configured state
    -- so effectively only the installed tarball packages get updated
    -- could also replace installed ones to be pre-installed

    putStrLn $ InstallPlan.showInstallPlan residualPlan
  where
    numJobs = determineNumJobs buildNumJobs

------------------------------------------------------------------------------
-- * Misc experimental prototype code
------------------------------------------------------------------------------

configure' :: Verbosity
          -> GlobalFlags
          -> ConfigFlags
          -> ConfigExFlags
          -> UseSandbox
          -> [String]
          -> IO ()
configure' verbosity
          globalFlags configFlags _configExFlags useSandbox
          _extraArgs = do

    print "New multi-pkg configure"
    localSrcPkgs <- resolveEnvironment
    print localSrcPkgs
    let userTargets = map UserTargetLocalDir ("." : localSrcPkgs)
        --TODO: ^^ do this properly
    print userTargets

    cabalDir   <- defaultCabalDir
    cacheDir   <- defaultCacheDir
    let cabalDirs@CabalDirLayout{..} = defaultCabalDirLayout cabalDir

    (compiler, platform, progdb) <- configCompilerAuxEx configFlags

    let ambientPackageDbs  = [GlobalPackageDB]
        storePackageDbs    = [GlobalPackageDB,
                              cabalStorePackageDB (compilerId compiler)]

    installedPkgIndex <- getInstalledPackages verbosity compiler
                                              ambientPackageDbs progdb
    --TODO: ensure store db exists before loading it
    storePkgIndex     <- getInstalledPackages verbosity compiler
                                              storePackageDbs   progdb
    sourcePkgDb       <- getSourcePackages    verbosity repos
    transport         <- configureTransport   verbosity preferredTransport

    pkgSpecifiers     <- resolveUserTargets verbosity transport
                           worldFile
                           (packageIndex sourcePkgDb)
                           userTargets

    let solverInputs = (compiler, platform,
--                        installedPkgIndex,
--                        selectNeededSubset sourcePkgDb (Set.fromList (map pkgSpecifierTarget pkgSpecifiers)),
                        pkgSpecifiers)
    solverInputsChanged <- checkValueChanged distSolverInputsCache solverInputs
    --TODO: minimise the solverInputs size to reduce spurious rebuilds?
    -- if so, has to be cheap since we'd check it each time
    
    --TODO: avoid even reading the source or installed indexes if nothing has changed
    --      so we get to keep the elaborated install plan

    installPlan <- case solverInputsChanged of
      Changed -> do
        notice verbosity "Resolving dependencies..."
        planResult <- foldProgress logMsg (return . Left) (return . Right)
                    $ planPackages compiler platform
                                   installedPkgIndex sourcePkgDb
                                   pkgSpecifiers
        case planResult of
          Left message      -> die message
          Right installPlan -> do
            putStrLn "updating solver inputs cache"
            updateValueChangeCache distSolverInputsCache
                                   solverInputs installPlan
            putStrLn "done"
            return installPlan

      Unchanged installPlan -> return installPlan

    return ()

  where
    repos              = globalRepos globalFlags
    preferredTransport = Cabal.flagToMaybe (globalHttpTransport globalFlags)
    worldFile          = Cabal.fromFlag (globalWorldFile globalFlags)

    resolveEnvironment =
      case useSandbox of
        NoSandbox -> return []
        UseSandbox _sandboxDir -> do
          indexFile     <- tryGetIndexFilePath SavedConfig { savedGlobalFlags = globalFlags }
          Index.listBuildTreeRefs verbosity
                                  Index.DontListIgnored
                                  Index.LinksAndSnapshots
                                  indexFile

    logMsg message rest = debugNoWrap verbosity message >> rest


build' :: Verbosity
      -> GlobalFlags
      -> ConfigFlags
      -> ConfigExFlags
      -> UseSandbox
      -> BuildFlags
      -> BuildExFlags
      -> [String]
      -> IO ()
build' verbosity
      globalFlags
      configFlags _configExFlags useSandbox
      _buildFlags _buildExFlags
      _extraArgs = do

    print "New multi-pkg build"

    localSrcPkgs <- resolveEnvironment
    print localSrcPkgs
    let userTargets = map UserTargetLocalDir ("." : localSrcPkgs)
        --TODO: ^^ do this properly
    print userTargets

    cabalDir   <- defaultCabalDir
    cacheDir   <- defaultCacheDir
    let cabalDirs@CabalDirLayout{..} = defaultCabalDirLayout cabalDir

    (compiler, platform, compilerProgs) <- configCompilerAuxEx configFlags

    let corePackageDbs  = [GlobalPackageDB]
        storePackageDbs = [GlobalPackageDB,
                           cabalStorePackageDB (compilerId compiler)]

    sourcePkgDbFingerprint <- getSourcePackagesFingerprint repos
    sourcePkgDbChanged :: Changed ()     <- checkValueChanged "sourcePkgDbFingerprint.cache" sourcePkgDbFingerprint
    print sourcePkgDbFingerprint
    print sourcePkgDbChanged

    installedPkgIndex <- getInstalledPackages verbosity compiler
                                              corePackageDbs compilerProgs

    sourcePkgDb       <- getSourcePackages    verbosity repos
    transport         <- configureTransport   verbosity preferredTransport

    pkgSpecifiers     <- resolveUserTargets verbosity transport
                           worldFile
                           (packageIndex sourcePkgDb)
                           userTargets

    let solverInputs = (compiler, platform,
--                        installedPkgIndex,
--                        selectNeededSubset sourcePkgDb (Set.fromList (map pkgSpecifierTarget pkgSpecifiers)),
                        pkgSpecifiers)
    solverInputsChanged <- checkValueChanged distSolverInputsCache solverInputs
    --TODO: minimise the solverInputs size to reduce spurious rebuilds?
    -- if so, has to be cheap since we'd check it each time
    
    --TODO: avoid even reading the source or installed indexes if nothing has changed
    --      so we get to keep the elaborated install plan

    installPlan <- case solverInputsChanged of
      Changed -> do
        notice verbosity "Resolving dependencies..."
        planResult <- foldProgress logMsg (return . Left) (return . Right)
                    $ planPackages compiler platform
                                   installedPkgIndex sourcePkgDb
                                   pkgSpecifiers
        case planResult of
          Left message      -> die message
          Right installPlan -> do
            putStrLn "updating solver inputs cache"
            updateValueChangeCache distSolverInputsCache
                                   solverInputs installPlan
            putStrLn "done"
            return installPlan

      Unchanged installPlan -> return installPlan

    --TODO: ensure store db exists before loading it
    storePkgIndex     <- getInstalledPackages verbosity compiler
                                              storePackageDbs compilerProgs

    return ()

  where
    repos              = globalRepos globalFlags
    preferredTransport = Cabal.flagToMaybe (globalHttpTransport globalFlags)
    worldFile          = Cabal.fromFlag (globalWorldFile globalFlags)

    resolveEnvironment =
      case useSandbox of
        NoSandbox -> return []
        UseSandbox _sandboxDir -> do
          indexFile     <- tryGetIndexFilePath SavedConfig { savedGlobalFlags = globalFlags }
          Index.listBuildTreeRefs verbosity
                                  Index.DontListIgnored
                                  Index.LinksAndSnapshots
                                  indexFile

    logMsg message rest = debugNoWrap verbosity message >> rest


-- | We don't need the entire index (which is rather large and costly if we
-- force it by examining the whole thing). So trace out the maximul subset of
-- each index that we could possibly ever need. Do this by flattening packages
-- and looking at the names of all possible dependencies.
--
selectNeededSubset :: SourcePackageDb
                   -> Set PackageName
                   -> SourcePackageDb
selectNeededSubset (SourcePackageDb sourcePkgIndex prefs) remaining0 =
    SourcePackageDb (select sourcePkgIndex remaining0) prefs
  where
    select :: Client.PackageIndex.PackageIndex SourcePackage
           -> Set PackageName
           -> Client.PackageIndex.PackageIndex SourcePackage
    select sourcePkgIndex' remaining
      | Set.null remaining = sourcePkgIndex'
      | otherwise          = select sourcePkgIndex'' remaining''
      where
        (next, remaining') = Set.deleteFindMin remaining
        sourcePkgIndex''   = foldl' (flip Client.PackageIndex.insert)
                                    sourcePkgIndex' moreSource
        remaining''        = foldl' (flip          Set.insert)
                                    remaining' moreRemaining
        moreSource    = Client.PackageIndex.lookupPackageName sourcePkgIndex next
        moreRemaining = -- we filter out packages already included in the indexes
                        -- this avoids an infinite loop if a package depends on itself
                        -- like base-3.0.3.0 with base-4.0.0.0
          [ name
          | SourcePackage _ pkg _ _ <- moreSource
          , name <- allDependencies pkg
          , null (Client.PackageIndex.lookupPackageName sourcePkgIndex' name)
          ]

    allDependencies pkg =
      [ pkgname
      | Dependency pkgname _ <-
            maybe [] treeConstraints (Cabal.condLibrary pkg)
         ++ concatMap (treeConstraints . snd) (Cabal.condExecutables pkg)
         ++ concatMap (treeConstraints . snd) (Cabal.condTestSuites pkg)
         ++ concatMap (treeConstraints . snd) (Cabal.condBenchmarks pkg)
      ]
    
    treeConstraints :: Cabal.CondTree v [c] a -> [c]
    treeConstraints node =
        Cabal.condTreeConstraints node
     ++ [ c | (_, t, _)      <- Cabal.condTreeComponents node
            , c <- treeConstraints t ]
     ++ [ c | (_, _, Just t) <- Cabal.condTreeComponents node
            , c <- treeConstraints t ]



-- Outline for build:
--
-- Establish what the environment is:
--   Read all the config files and merge them.
--     ~/.cabal/config, cabal.config and cabal.local.config
-- Separate the environment into those bits that are used as input to the
--   solver, and the bits that are used only in the build without affecting
--   the solver.
-- Resolve if necessary
--   Prune (and normalise) the input to the solver so that we check the minimum
--   amount of the environment for changes, e.g. take a dep closure of the
--   installed and source packages so that installing other global packages
--   doesn't trigger solving again. We only want to re-solve when the solver
--   could possibly have picked a different solution.
-- make 


testRepo :: FilePath -> Repo
testRepo cacheDir = Repo (Left defaultRemoteRepo)
                         (cacheDir </> remoteRepoName defaultRemoteRepo)


-- | Test the new building code
--
testBuild :: [UserTarget] -> IO ()
testBuild userTargets = do

    cacheDir   <- defaultCacheDir

    let ambientPackageDbs = [GlobalPackageDB]
        storePackageDbs   = [GlobalPackageDB,
                             SpecificPackageDB "/home/duncan/.cabal/store/package.db/ghc-7.10.1"]
                            --TODO: use per-compiler-id package db within the store
        repos      = [testRepo cacheDir]
        progdb0    = userSpecifyPath "ghc" "ghc-7.10.1" $
                     defaultProgramConfiguration


    (compiler, platform, progdb) <- configCompilerEx (Just GHC) Nothing Nothing
                                                     progdb0 verbosity

    installedPkgIndex <- getInstalledPackages verbosity compiler ambientPackageDbs progdb
    --TODO: ensure store db exists before loading it
    storePkgIndex     <- getInstalledPackages verbosity compiler storePackageDbs   progdb
    sourcePkgDb       <- getSourcePackages    verbosity repos

    transport         <- configureTransport verbosity Nothing

    pkgSpecifiers     <- resolveUserTargets verbosity transport
                           "no-world-file"
                           (packageIndex sourcePkgDb)
                           userTargets

    notice verbosity "Resolving dependencies..."
    planResult     <- foldProgress logMsg (return . Left) (return . Right)
                    $ planPackages compiler platform
                                   installedPkgIndex sourcePkgDb
                                   pkgSpecifiers

    case planResult of
      Left message      -> die message
      Right installPlan -> do

        putStrLn $ InstallPlan.showInstallPlan installPlan

        -- Now we do some post-processing on the install plan
        -- Firstly we set which packages are going to be local builds.
        -- Then the important nix part: we take source packages that the
        -- solver has picked and see if we can replace any of them with 

        -- TODO: once we have the security system inplace we can get SHA256
        -- hashes from the repo. Use these when available so that we can avoid
        -- computing them all.
        cwd <- getCurrentDirectory

        let (elaboratedInstallPlan, sharedPackageConfig) =
              elaborateInstallPlanForLocalBuild
                platform compiler progdb
                ambientPackageDbs storePackageDbs
                cwd
                installPlan
                pkgSpecifiers

        putStrLn $ InstallPlan.showInstallPlan elaboratedInstallPlan
        putStrLn $ printPlan elaboratedInstallPlan

        elaboratedInstallPlan' <-
          reusePackagesFromStore
            pkgSourceLocation
            (elaboratedPackageHashConfigInputs sharedPackageConfig)
            storePkgIndex
            elaboratedInstallPlan

        putStrLn $ InstallPlan.showInstallPlan elaboratedInstallPlan'
        putStrLn $ printPlan elaboratedInstallPlan'
--        mapM_ print [ pkg | InstallPlan.Configured pkg <- InstallPlan.toList elaboratedInstallPlan' ]

        residualPlan <- buildTargets
                          verbosity
                          1 -- build jobs
                          transport
                          sharedPackageConfig
                          elaboratedInstallPlan'

        --TODO: this result plan can be saved, resetting all failed and
        -- BuildInplaceOnly packages to original Configured state
        -- so effectively only the installed tarball packages get updated
        -- could also replace installed ones to be pre-installed

        putStrLn $ InstallPlan.showInstallPlan residualPlan

  where
    verbosity = normal --verbose
    logMsg message rest = debugNoWrap verbosity message >> rest






--debug:

--eqPlanPackage (InstallPlan.PreExisting x) (InstallPlan.PreExisting y) = packageId x == packageId y && depends x == depends y
--eqPlanPackage (InstallPlan.Configured  x) (InstallPlan.Configured  y) = x == y


-- ------------------------------------------------------------
-- * Installation planning
-- ------------------------------------------------------------

planPackages :: Compiler
             -> Platform
--             -> ConfigFlags
--             -> ConfigExFlags
--             -> InstallFlags
             -> InstalledPackageIndex
             -> SourcePackageDb
             -> [PackageSpecifier SourcePackage]
             -> Progress String String InstallPlan
planPackages comp platform
             --configFlags configExFlags installFlags
             installedPkgIndex sourcePkgDb pkgSpecifiers =

    resolveDependencies
      platform (compilerInfo comp)
      Modular
      resolverParams

  where

    resolverParams =

        setMaxBackjumps Nothing

      . setIndependentGoals False

      . setReorderGoals False

      . setAvoidReinstalls False

      . setShadowPkgs False -- don't shadow for installing into user store

      . setStrongFlags False

      . setPreferenceDefault (if True then PreferAllLatest
                                      else PreferLatestForSelected)
                             --TODO: decide if we need to prefer installed for global packages?

      . removeUpperBounds AllowNewerNone

      . addPreferences []

      . reinstallTargets  --TODO: do we want this? or just hide all installed packages in the store from the solver

      $ standardInstallPolicy
        installedPkgIndex sourcePkgDb pkgSpecifiers

------------------------------------------------------------------------------
-- * Install plan post-processing
------------------------------------------------------------------------------

-- This phase goes from the InstallPlan we get from the solver and has to
-- make an elaborated install plan.
--
-- We go in two steps:
--
--  1. elaborate all the source packages that the solver has chosen.
--  2. swap source packages for pre-existing installed packages wherever
--     possible.
--
-- We do it in this order, elaborating and then replacing, because the easiest
-- way to calculate the installed package ids used for the replacement step is
-- from the elaborated configuration for each package.




------------------------------------------------------------------------------
-- * Install plan elaboration
------------------------------------------------------------------------------

-- | Produce an elaborated install plan using the policy for local builds with
-- a nix-style shared store.
--
-- In theory should be able to make an elaborated install plan with a policy
-- matching that of the classic @cabal install --user@ or @--global@
--
elaborateInstallPlanForLocalBuild
  :: Platform -> Compiler -> ProgramDb
  -> PackageDBStack -> PackageDBStack
  -> FilePath
  -> GenericInstallPlan InstalledPackageInfo
                        ConfiguredPackage
                        _iresult _ifailure
  -> [PackageSpecifier SourcePackage]
  -> (ElaboratedInstallPlan, ElaboratedSharedConfig)
elaborateInstallPlanForLocalBuild platform compiler progdb
                                  ambientPackageDbs storePackageDbs
                                  -- TODO: ^^^ these args could come as flag
                                  -- sets or other config file input. For now
                                  -- just individual args
                                  cwd
                                  solverPlan pkgSpecifiers =
    (installPlan, sharedPackageConfig)
  where
    sharedPackageConfig =
      ElaboratedSharedConfig {
        pkgConfigPlatform         = platform,
        pkgConfigCompiler         = compiler,
        pkgConfigProgramDb        = progdb,
 
        --TODO: for now just an example and not yet configurable
        pkgConfigLibraryProfiling = False
      }

    installPlan = InstallPlan.mapPreservingGraph convertPlanPackage solverPlan

    convertPlanPackage :: InstallPlan.GenericPlanPackage InstalledPackageInfo
                                                         ConfiguredPackage
                                                         _iresult _ifailure
                       -> InstallPlan.GenericPlanPackage InstalledPackageInfo
                                                         ElaboratedConfiguredPackage
                                                         iresult ifailure
    convertPlanPackage (InstallPlan.PreExisting pkg) =
      InstallPlan.PreExisting pkg

    convertPlanPackage (InstallPlan.Configured  pkg) =
      InstallPlan.Configured (individualPackageConfig pkg)

    convertPlanPackage _ =
      error "elaborateInstallPlanForLocalBuild: unexpected package state"
    
    individualPackageConfig :: ConfiguredPackage -> ElaboratedConfiguredPackage
    individualPackageConfig
      pkg@(ConfiguredPackage (SourcePackage pkgid desc srcloc descOverride)
                             flags stanzas deps) =

      ElaboratedConfiguredPackage {
        pkgDescription      = desc,
        pkgSrcId            = pkgid,
        pkgFlagAssignment   = flags,
        pkgEnabledStanzas   = stanzas,
        pkgDependencies     = fmap (map confInstId) deps,
        pkgSourceLocation = srcloc,
        pkgBuildStyle       = if shouldBuildInplaceOnly pkg
                                then BuildInplaceOnly else BuildAndInstall,
        pkgSetupPackageDBStack    = ambientPackageDbs,
                                    --TODO: ^^ this should be conditional on
                                    -- whether the package is using an explicit
                                    -- custom setup stanza for specific deps.
                                    -- If so, then it must use the storeDb.
        pkgBuildPackageDBStack    = buildAndRegisterDbs,
        pkgRegisterPackageDBStack = buildAndRegisterDbs,
        pkgRequiresRegistration   = isJust (Cabal.condLibrary desc),
        pkgDescriptionOverride    = descOverride
      }
      where
        --TODO: does the BuildInplaceOnly affect our choice of registration db here?
        buildAndRegisterDbs
          | shouldBuildInplaceOnly pkg
                      = storePackageDbs
                         ++ [SpecificPackageDB (cwd </> distPackageDB)]
          | otherwise = storePackageDbs

    -- For this local build policy, every package that lives in a local source
    -- dir (as opposed to a tarball), or depends on such a package, will be
    -- built inplace into a shared dist dir. Tarball packages that depend on
    -- source dir packages will also get unpacked locally.
    shouldBuildInplaceOnly :: HasInstalledPackageId pkg => pkg -> Bool
    shouldBuildInplaceOnly pkg = Set.member (installedPackageId pkg)
                                            pkgsToBuildInplaceOnly

    pkgsToBuildInplaceOnly :: Set InstalledPackageId
    pkgsToBuildInplaceOnly =
        Set.fromList
      $ map installedPackageId
      $ InstallPlan.reverseDependencyClosure
          solverPlan
          [ fakeInstalledPackageId (packageId pkg)
          | SpecificSourcePackage pkg <- pkgSpecifiers ]


------------------------------------------------------------------------------
-- * Sharing installed packages
------------------------------------------------------------------------------

--
-- Nix style store management for tarball packages
--
-- So here's our strategy:
--
-- We use a per-user nix-style hashed store, but /only/ for tarball packages.
-- So that includes packages from hackage repos (and other http and local
-- tarballs). For packages in local directories we do not register them into
-- the shared store by default, we just build them locally inplace.
--
-- The reason we do it like this is that it's easy to make stable hashes for
-- tarball packages, and these packages benefit most from sharing. By contrast
-- unpacked dir packages are harder to hash and they tend to change more
-- frequently so there's less benefit to sharing them.
--
-- When using the nix store approach we have to run the solver *without*
-- looking at the packages installed in the store, just at the source packages
-- (plus core\/global installed packages). Then we do a post-processing pass
-- to replace configured packages in the plan with pre-existing ones, where
-- possible. Where possible of course means where the nix-style package hash
-- equals one that's already in the store.
--
-- One extra wrinkle is that unless we know package tarball hashes upfront, we
-- will have to download the tarballs to find their hashes. So we have two
-- options: delay replacing source with pre-existing installed packages until
-- the point during the execution of the install plan where we have the
-- tarball, or try to do as much up-front as possible and then check again
-- during plan execution. The former isn't great because we would end up
-- telling users we're going to re-install loads of packages when in fact we
-- would just share them. It'd be better to give as accurate a prediction as
-- we can. The latter is better for users, but we do still have to check
-- during plan execution because it's important that we don't replace existing
-- installed packages even if they have the same package hash, because we
-- don't guarantee ABI stability.

-- TODO: for safety of concurrent installs, we must make sure we register but
-- not replace installed packages with ghc-pkg.

elaboratedPackageHashConfigInputs :: ElaboratedSharedConfig
                                  -> ElaboratedConfiguredPackage
                                  -> PackageHashConfigInputs
elaboratedPackageHashConfigInputs
    ElaboratedSharedConfig{..}
    ElaboratedConfiguredPackage{..} =

    PackageHashConfigInputs {
      pkgHashLibraryProfiling = pkgConfigLibraryProfiling
      --TODO: all of them
    }

reusePackagesFromStore :: (PackageFixedDeps srcpkg, HasInstalledPackageId srcpkg)
                       => (srcpkg -> PackageLocation (Maybe FilePath))
                       -> (srcpkg -> PackageHashConfigInputs)
                       -> InstalledPackageIndex
                       -> GenericInstallPlan InstalledPackageInfo srcpkg
                                      iresult ifailure
                       -> IO (GenericInstallPlan InstalledPackageInfo srcpkg
                                          iresult ifailure)
reusePackagesFromStore packageSourceLocation
                       packageHashConfigInputs
                       storePkgIndex installPlan = do

    pkgsAndTarballs <-
      packagesWithFetchedTarballs
        packageSourceLocation
        [ pkg | InstallPlan.Configured pkg <- InstallPlan.toList installPlan ]

    knownSrcPkgHashInfo <-
      liftM Map.fromList $
      sequence
        [ do srchash <- readFileHashValue tarball
             let conf = packageHashConfigInputs pkg
             return (packageId pkg, (srchash, conf))

        | (pkg, tarball) <- pkgsAndTarballs ]

    let installPlan' = improveInstallPlanWithPreExistingPackages
                         storePkgIndex
                         knownSrcPkgHashInfo
                         installPlan

    return installPlan'

packagesWithFetchedTarballs :: (srcpkg -> PackageLocation (Maybe FilePath))
                            -> [srcpkg]
                            -> IO [(srcpkg, FilePath)]
packagesWithFetchedTarballs packageSourceLocation pkgs = do
    pkgslocs <- sequence
                  [ do mloc <- checkFetched (packageSourceLocation pkg)
                       return (pkg, mloc)
                  | pkg <- pkgs
                  ]
    return [ (pkg, tarloc)
           | (pkg, Just srcloc) <- pkgslocs
           , tarloc <- maybeToList (getTarballFile srcloc)
           ]
  where
    getTarballFile (LocalUnpackedPackage _dir)      = Nothing
    getTarballFile (LocalTarballPackage    tarball) = Just tarball
    getTarballFile (RemoteTarballPackage _ tarball) = Just tarball
    getTarballFile (RepoTarballPackage _ _ tarball) = Just tarball


-- | Given the 'InstalledPackageIndex' for a nix-style package store, and
-- enough information to calculate 'InstalledPackageId' for a selection of
-- source packages 
-- 
improveInstallPlanWithPreExistingPackages
  :: forall srcpkg iresult ifailure.
     (HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
  => InstalledPackageIndex
  -> Map PackageId (PackageSourceHash, PackageHashConfigInputs)
  -> GenericInstallPlan InstalledPackageInfo srcpkg iresult ifailure
  -> GenericInstallPlan InstalledPackageInfo srcpkg iresult ifailure
improveInstallPlanWithPreExistingPackages
    installedPkgIndex knownSrcPkgHashInfo =

    go []
  where
    -- So here's the strategy:
    --
    --  * Go through each ready package in dependency order. Indeed we
    --    simulate executing the plan, but instead of going from ready to
    --    processing to installed, we go from ready to either pre-existing
    --    or processing.
    --
    --  * Calculate the 'InstalledPackageId' if we can (ie if we've been able
    --    to get the tarball hash)
    --
    --  * Check if that package is already installed and if so, we replace the
    --    ready pacage by the pre-existing package.
    --
    --  * If we cannot calculate the 'InstalledPackageId' or it's not already
    --    installed (ie we would have to build it) then we put it into the
    --    'Processing' state so that it doesn't keep appearing in the ready
    --    list.
    --
    --  * When there are no more packages in the ready state then we're done,
    --    except that we need to reset the packages we put into the processing
    --    state.
    --
    -- When we have ready packages that we cannot replace with pre-existing
    -- packages then none of their dependencies can be replaced either. This
    -- constraint is respected here because we put those packages into the
    -- processing state, and so none of their deps will be able to appear in
    -- the ready list.
    --
    -- We accumulate the packages in the processing state. These are the ones
    -- that will have to be built because they cannot be replaced with
    -- pre-existing installed packages.

    go :: [GenericReadyPackage srcpkg InstalledPackageInfo]
       -> GenericInstallPlan InstalledPackageInfo srcpkg iresult ifailure
       -> GenericInstallPlan InstalledPackageInfo srcpkg iresult ifailure
    go cannotBeImproved installPlan =
      traceShow ("go", map (display . packageId) cannotBeImproved) $
      case InstallPlan.ready installPlan of
        -- no more source packages can be replaced with pre-existing ones,
        --  just need to reset the ones we put into the processing state
        [] -> trace "go done" $
              InstallPlan.reverted
                [ pkg | ReadyPackage pkg _ <- cannotBeImproved ]
                installPlan

        -- we have some to look at
        pkgs -> traceShow ("go ready pkgs", map (display . packageId) pkgs) $
                traceShow ("go canBeImproved", map (display . packageId . fst) canBeImproved) $
                traceShow ("go cannotBeImproved'", map (display . packageId) cannotBeImproved') $
                go (cannotBeImproved' ++ cannotBeImproved)
                   installPlan'
          where
            installPlan' = InstallPlan.processing cannotBeImproved'
                         . replaceWithPreExisting canBeImproved
                         $ installPlan

            (cannotBeImproved', canBeImproved) =
              partitionEithers
                [ case canPackageBeImproved pkg of
                    Nothing   -> Left pkg
                    Just ipkg -> Right (pkg, ipkg)
                | pkg <- pkgs ]

    canPackageBeImproved :: GenericReadyPackage srcpkg InstalledPackageInfo
                         -> Maybe InstalledPackageInfo
    canPackageBeImproved pkg = do
      (srcHash, configHash) <- Map.lookup (packageId pkg) knownSrcPkgHashInfo
      let pkgHashInputs = PackageHashInputs {
              pkgHashPkgId         = packageId pkg,
              pkgHashSourceHash    = srcHash ,
              pkgHashDirectDeps    = ComponentDeps.libraryDeps (depends pkg), --TODO: consider carefully which deps
              pkgHashOtherConfig   = configHash
            } 
          ipkgid = hashedInstalledPackageId pkgHashInputs
      PackageIndex.lookupInstalledPackageId installedPkgIndex ipkgid

    replaceWithPreExisting :: [(GenericReadyPackage srcpkg InstalledPackageInfo, InstalledPackageInfo)]
                           -> GenericInstallPlan InstalledPackageInfo srcpkg iresult ifailure
                           -> GenericInstallPlan InstalledPackageInfo srcpkg iresult ifailure
    replaceWithPreExisting canBeImproved plan0 =
      foldl' (\plan (pkg, ipkg) -> InstallPlan.preexisting (installedPackageId pkg) ipkg plan)
             plan0
             canBeImproved


------------------------------------------------------------------------------



-- We want to execute the install plan, which may involve installing packages
-- in the classic way, or building packages inplace.
--
buildTargets :: Verbosity
             -> Int -> HttpTransport
             -> ElaboratedSharedConfig
             -> ElaboratedInstallPlan
--           -> [BuildTarget] --TODO: only build the ones we ask for out of the persistent plan
             -> IO ElaboratedInstallPlan
buildTargets verbosity
             numBuildJobs transport 
             sharedPackageConfig
             installPlan = do

  -- Concurrency control: create the job controler and concurrency limits 
  -- for downloading, building and installing.
  jobControl    <- if parallelInstall then newParallelJobControl
                                      else newSerialJobControl
  buildLimit    <- newJobLimit numBuildJobs
  downloadLimit <- newJobLimit (min numBuildJobs numDownloadJobs)
  installLock   <- newLock -- serialise installation
  cacheLock     <- newLock -- serialise access to setup exe cache

  createDirectoryIfMissingVerbose verbosity False distDirectory
  createDirectoryIfMissingVerbose verbosity False distBuildRootDirectory

  -- Before traversing the install plan, pre-emptively find all packages that
  -- will need to be downloaded and start downloading them.
  pkgsToDownload <- packagesRequiringDownload
                      pkgSourceLocation
                      installPlan

  mapM_ print pkgsToDownload

  asyncDownloadPackages verbosity transport downloadLimit
                        pkgsToDownload $ \downloadMap ->

    -- For each package in the plan, in dependency order, but in parallel...
    executeInstallPlan verbosity jobControl installPlan
                       $ \rpkg@(ReadyPackage (cpkg :: ElaboratedConfiguredPackage) _) -> do

      -- If necessary, wait for the download of the remote package to finish.
      srcloc <- waitAsyncPackageDownload verbosity downloadMap cpkg

      withJobLimit buildLimit $ --TODO: think about this carefully
        withPackageInLocalDirectory verbosity srcloc (pkgBuildStyle cpkg)
                                    (packageId rpkg)
                                    $ \srcdir builddir mtarball -> do
          case (pkgBuildStyle cpkg, mtarball) of
            (BuildAndInstall, Just tarball) -> do
              --TODO: don't recalculate this if we don't need to
              srchash <- readFileHashValue tarball
              let pkgHashInputs = PackageHashInputs {
                      pkgHashPkgId         = packageId rpkg,
                      pkgHashSourceHash    = srchash,
                      pkgHashDirectDeps    = ComponentDeps.libraryDeps (depends rpkg), --TODO: consider carefully which deps
                      pkgHashOtherConfig   = elaboratedPackageHashConfigInputs
                                               sharedPackageConfig cpkg
                    }
                  ipkgid = hashedInstalledPackageId pkgHashInputs

              buildAndInstallUnpackedPackage
                verbosity installLock cacheLock
                sharedPackageConfig
                rpkg ipkgid srcdir builddir'
              where
                builddir' = makeRelative srcdir builddir
                --TODO ^^ do this relative stuff better

            (BuildInplaceOnly, _) -> do
              --TODO: ensure the store db is initialised
              --TODO: use a relative build dir rather than absolute
              builddir' <- canonicalizePath builddir
              printTiming "=========== buildInplaceUnpackedPackage ============" $
                buildInplaceUnpackedPackage
                  verbosity installLock cacheLock
                  sharedPackageConfig
                  rpkg srcdir builddir'

            (BuildAndInstall, Nothing) ->
              fail "internal error: BuildAndInstall mode without tarball"

  where
    numDownloadJobs = 2
    parallelInstall = numBuildJobs >= 2

printTiming :: Show t => t -> IO b -> IO b
printTiming name action = do
  before <- getCurrentTime
  x <- action
  after <- getCurrentTime
  print ("printTiming", name, after `diffUTCTime` before)
  return x

--TODO: do we need to use a with-style for the temp files for downloading http
-- packages, or are we going to cache them persistently?

-- | Given the current 'InstallPlan', work out which packages will require
-- a download step of some form, and return the locations of those packages,
-- in the dependency order (so that the one's we'll need first are the ones
-- we will start downloading first).
--
packagesRequiringDownload :: (srcpkg -> PackageLocation (Maybe FilePath))
                          -> GenericInstallPlan ipkg srcpkg iresult ifailure
                          -> IO [PackageLocation (Maybe FilePath)]
packagesRequiringDownload packageSourceLocation installPlan =
    filterM (fmap not . isFetched)
      [ packageSourceLocation pkg
      | InstallPlan.Configured pkg
         <- InstallPlan.reverseTopologicalOrder installPlan
      ]
      --TODO: check that the reverseTopologicalOrder is more or less the same
      -- order as we get out of the install plan when we run it

type AsyncDownloadMap = Map (PackageLocation (Maybe FilePath))
                            (Async (PackageLocation FilePath))

-- | Fork off async actions to download all the given packages (subject to the
-- given concurrency limit). The body action is passed a map from those
-- packages (identified by their location) to the async action that's doing
-- the download for that package. So the body action should lookup the location
-- and use 'Async.wait' or 'waitAsyncPackageDownload' to get the result.
--
asyncDownloadPackages :: Verbosity
                      -> HttpTransport
                      -> JobLimit
                      -> [PackageLocation (Maybe FilePath)]
                      -> (AsyncDownloadMap -> IO a)
                      -> IO a
asyncDownloadPackages _ _ _ [] body = body Map.empty
asyncDownloadPackages verbosity transport fetchLimit pkglocs0 body =
    go Map.empty pkglocs0
  where
    -- We want to use 'withAsync' to get the automatic cancellation behaviour,
    -- so that means we have to nest them all inside each other.
    go !downloadMap []               = body downloadMap
    go !downloadMap (pkgloc:pkglocs) = do
      let downloadAction =
            withJobLimit fetchLimit $
              fetchPackage transport verbosity pkgloc

      withAsync downloadAction $ \asyncHandle -> do
        let downloadMap' = Map.insert pkgloc asyncHandle downloadMap
        go downloadMap' pkglocs
    --TODO: this isn't quite what we want because with the jobs being started
    -- in any order, there's nothing to ensure we download the more important
    -- packages first.
    -- Perhaps should use the JobControl abstraction instead?


-- | Check if a package needs downloading, and if so expect to find a download
-- in progress in the given 'AsyncDownloadMap' and wait on it to finish.
--
waitAsyncPackageDownload :: Verbosity
                         -> AsyncDownloadMap
                         -> ElaboratedConfiguredPackage
                         -> IO (PackageLocation FilePath)
waitAsyncPackageDownload verbosity downloadMap pkg = do
    mloc <- checkFetched (pkgSourceLocation pkg)
    case mloc of
      Nothing  -> do let Just hnd = Map.lookup (pkgSourceLocation pkg) downloadMap
                     debug verbosity $ "Waiting for download of "
                                    ++ display (packageId pkg)
                                    ++ " to finish"
                     wait hnd
      Just loc -> return loc
--TODO: do the exception handling on download stuff


-- for the build cache we use one of two methods:
--  for build type Simple we assume we do actually know the files in the package
--  for other build types we look at all files, only excluding known boring

type BuildResult' ipkg iresult ifailure = Either ifailure (Maybe ipkg, iresult)  --TODO: rename:

--TODO: generalise BuildFailure
executeInstallPlan :: forall ipkg srcpkg iresult.
                      (HasInstalledPackageId ipkg,   PackageFixedDeps ipkg,
                       HasInstalledPackageId srcpkg, PackageFixedDeps srcpkg)
                   => Verbosity
                   -> JobControl IO (PackageId, BuildResult' ipkg iresult BuildFailure)
                   -> GenericInstallPlan ipkg srcpkg iresult BuildFailure
                   -> (GenericReadyPackage srcpkg ipkg
                      -> IO (BuildResult' ipkg iresult BuildFailure))
                   -> IO (GenericInstallPlan ipkg srcpkg iresult BuildFailure)
executeInstallPlan verbosity jobCtl plan0 installPkg =
    tryNewTasks 0 plan0
  where
    tryNewTasks taskCount plan = do
      case InstallPlan.ready plan of
        [] | taskCount == 0 -> return plan
           | otherwise      -> waitForTasks taskCount plan
        pkgs                -> do
          sequence_
            [ do info verbosity $ "Ready to install " ++ display pkgid
                 spawnJob jobCtl $ do
                   buildResult <- installPkg pkg
                   return (packageId pkg, buildResult)
            | pkg <- pkgs
            , let pkgid = packageId pkg
            ]

          let taskCount' = taskCount + length pkgs
              plan'      = InstallPlan.processing pkgs plan
          waitForTasks taskCount' plan'

    waitForTasks taskCount plan = do
      info verbosity $ "Waiting for install task to finish..."
      (pkgid, buildResult) <- collectJob jobCtl
      let taskCount' = taskCount-1
          plan'      = updatePlan pkgid buildResult plan
      tryNewTasks taskCount' plan'

    updatePlan :: PackageIdentifier
               -> BuildResult' ipkg iresult BuildFailure
               -> GenericInstallPlan ipkg srcpkg iresult BuildFailure
               -> GenericInstallPlan ipkg srcpkg iresult BuildFailure
    updatePlan pkgid (Right (mipkg, buildSuccess)) =
        InstallPlan.completed (fakeInstalledPackageId pkgid) mipkg buildSuccess

    updatePlan pkgid (Left buildFailure) =
        InstallPlan.failed (fakeInstalledPackageId pkgid) buildFailure depsFailure
      where
        depsFailure = DependentFailed pkgid
        -- So this first pkgid failed for whatever reason (buildFailure).
        -- All the other packages that depended on this pkgid, which we
        -- now cannot build, we mark as failing due to 'DependentFailed'
        -- which kind of means it was not their fault.


-- | Ensure that the package is unpacked in an appropriate directory, either
-- a temporary one or a persistent one under the shared dist directory. 
--
withPackageInLocalDirectory
  :: Verbosity
  -> PackageLocation FilePath
  -> BuildStyle
  -> PackageIdentifier
  -> (FilePath -> FilePath -> Maybe FilePath -> IO a)
  -> IO a
withPackageInLocalDirectory verbosity location buildstyle pkgid buildPkg =

    case location of

      -- For the case of a user-managed local dir, irrespective of the build
      -- style, we build from that directory and put build artifacts under the
      -- shared dist directory.
      LocalUnpackedPackage srcdir ->
        buildPkg srcdir (distBuildDirectory pkgid) Nothing

      -- The three tarball cases are handled the same as each other,
      -- though depending on the build style.
      LocalTarballPackage    tarball -> withTarballLocalDirectory tarball
      RemoteTarballPackage _ tarball -> withTarballLocalDirectory tarball
      RepoTarballPackage _ _ tarball -> withTarballLocalDirectory tarball

  where
    withTarballLocalDirectory tarball =
      case buildstyle of
        -- In this case we make a temp dir, unpack the tarball to there and
        -- build and install it from that temp dir.
        BuildAndInstall -> do
        -- TODO: this is also a case where we can calculate the installed pkgid
          createDirectoryIfMissingVerbose verbosity False distTempDirectory
          withTempDirectory verbosity distTempDirectory
                            (display (packageName pkgid)) $ \tmpdir -> do
            extractTarballPackage verbosity tarball tmpdir pkgid 
            let srcdir   = tmpdir </> display pkgid
                builddir = srcdir </> "dist"
            buildPkg srcdir builddir (Just tarball)

        -- In this case we make sure the tarball has been unpacked to the
        -- appropriate location under the shared dist dir, and then build it
        -- inplace there
        BuildInplaceOnly -> do
          -- TODO: do a timestamp comparison and re-unpack if the tarball is updated
          -- TODO: re-overwrite the .cabal file if it gets updated in the repo
          let srcrootdir = distUnpackedSrcRootDirectory
              srcdir     = distUnpackedSrcDirectory pkgid
              builddir   = distBuildDirectory pkgid
          exists <- doesDirectoryExist srcdir
          unless exists $ do
            createDirectoryIfMissingVerbose verbosity False srcrootdir
            extractTarballPackage verbosity tarball srcrootdir pkgid
          buildPkg srcdir builddir (Just tarball)


extractTarballPackage :: Verbosity -> FilePath -> FilePath -> PackageId -> IO ()
extractTarballPackage verbosity tarball parentdir pkgid = do
    info verbosity $ "Extracting " ++ tarball ++ " to " ++ parentdir ++ "..."
    Tar.extractTarGzFile parentdir pkgsubdir tarball

    -- sanity check:
    exists <- doesFileExist cabalFile
    when (not exists) $
      die $ "Package .cabal file not found: " ++ show cabalFile
    
    --TODO: overwrite the .cabal file with the one from the index, when appropriate
    --TODO: move any pre-shipped dist dir into the right place
  where
    cabalFile = parentdir </> pkgsubdir
                          </> display (packageName pkgid) <.> "cabal"
    pkgsubdir = display pkgid

-- Override the .cabal file if necessary
{-
overridePackageCabalFile ... =
  case pkgoverride of
    Nothing     -> return ()
    Just pkgtxt -> do
      let descFilePath = fromMaybe "." workingDir
                     </> display (packageName pkgid) <.> "cabal"
      info verbosity $
        "Updating " ++ display (packageName pkgid) <.> "cabal"
                    ++ " with the latest revision from the index."
      writeFileAtomic descFilePath pkgtxt
-}

buildAndInstallUnpackedPackage :: Verbosity -> Lock -> Lock
                               -> ElaboratedSharedConfig
                               -> ElaboratedReadyPackage
                               -> InstalledPackageId
                               -> FilePath -> FilePath
                               -> IO (BuildResult' InstalledPackageInfo BuildSuccess BuildFailure)
buildAndInstallUnpackedPackage verbosity installLock _cacheLock
                               ElaboratedSharedConfig {
                                 pkgConfigPlatform  = platform,
                                 pkgConfigCompiler  = compiler,
                                 pkgConfigProgramDb = progdb
                               }
                               rpkg@(ReadyPackage
                                 pkg@ElaboratedConfiguredPackage {
                                   pkgSetupPackageDBStack,
                                   pkgBuildPackageDBStack,
                                   pkgRegisterPackageDBStack
                                 }
                                 deps)
                               ipkgid srcdir builddir = do

    putStrLn $ "buildAndInstallUnpackedPackage: " ++ srcdir ++ " " ++ builddir
    createDirectoryIfMissingVerbose verbosity False builddir

    --TODO: deal consistently with talking to older Setup.hs versions, much like
    --      we do for ghc, with a proper options type and rendering step
    --      which will also let us call directly into the lib, rather than always
    --      going via the lib's command line interface, which would also allow
    --      passing data like installed packages, compiler, and program db for a
    --      quicker configure.

    --TODO: optional logging
    --TODO: exception handling to tag things with the phase they failed in
    --TODO: docs and tests
    --TODO: win32-self-replacement
    --TODO: sudo re-exec

    -- Configure phase
    notice verbosity $ "Configuring " ++ display pkgid ++ "..."
    configureFlags' <- addDefaultInstallDirs configureFlags --TODO: use CabalDirLayout as part of env
    setup configureCommand' (\_ -> configureFlags')

    -- Build phase
    notice verbosity $ "Building " ++ display pkgid ++ "..."
    setup buildCommand' buildFlags

    -- Install phase
    criticalSection installLock $ do
      --TODO: do we need the installLock for copying? can we not do that in
      -- parallel? Isn't it just registering that we have to lock for?
      --TODO: need to lock installing this ipkig so other processes don't
      -- stomp on our files, since we don't have ABI compat, not safe to replace

      -- Actual installation
      setup Cabal.copyCommand copyFlags
      -- here's where we could keep track of the installed files ourselves if
      -- we wanted by calling copy to an image dir and then we would make a
      -- manifest and move it to its final location
      --TODO: we should actually have it make an image in store/incomming and
      -- then when it's done, move it to its final location, to reduce problems
      -- with installs failing half-way. Could also register and then move.

      -- For libraries, grab the package configuration file
      -- and register it ourselves
      mipkg <- generateInstalledPackageInfo
      case mipkg of
        Nothing   -> return ()
        Just ipkg -> do
          -- We register ourselves rather than via Setup.hs, because it's a bit
          -- cleaner (Setup.hs isn't allowed to expect to be able to modify the
          -- target system during register, it must be able to make a reg file).

          --TODO: this is a gross hack:
          let hcinf0 = GHC.hcPkgInfo progdb
              hcinf  = hcinf0 {
                         HcPkg.hcPkgProgram = (HcPkg.hcPkgProgram hcinf0) {
                             programOverrideArgs = ["--enable-multi-instance", "--force"]
                           }
                       }
          HcPkg.register hcinf verbosity pkgRegisterPackageDBStack (Right ipkg)

          --TODO: do this in a compiler agnostic way via Cabal lib
          --setup Cabal.registerCommand registerFlags

      let docsResult  = DocsNotTried
          testsResult = TestsNotTried
      return (Right (mipkg, BuildOk docsResult testsResult mipkg))

  where
    pkgid = packageId rpkg

    --FIXME: this is rediculous:
    pkgdesc = case Cabal.finalizePackageDescription
                     (pkgFlagAssignment pkg)
                     (const True)
                     platform (compilerInfo compiler) []
                     (enableStanzas (pkgEnabledStanzas pkg)
                                    (pkgDescription pkg))
                of Left _ -> error "finalizePackageDescription ReadyPackage failed"
                   Right (desc, _) -> desc


    configureCommand'= Cabal.configureCommand defaultProgramConfiguration
    --TODO: proper set of config options, not just Setup.hs interface flags
    configureFlags   = Cabal.emptyConfigFlags {
      Cabal.configDistPref  = toFlag builddir,
      Cabal.configVerbosity = toFlag verbosity,

      Cabal.configConfigurationsFlags = pkgFlagAssignment pkg,
      -- We generate the legacy constraints as well as the new style precise deps.
      -- In the end only one set gets passed to Setup.hs configure, depending on
      -- the Cabal version we are talking to.
--      Cabal.configConstraints  = [ thisPackageVersion (packageId deppkg)
--                                 | deppkg <- ComponentDeps.nonSetupDeps deps ],
      Cabal.configDependencies = [ (packageName (Installed.sourcePackageId deppkg),
                                    Installed.installedPackageId deppkg)
                                 | deppkg <- ComponentDeps.nonSetupDeps deps ],
      -- Use '--exact-configuration' if supported.
      Cabal.configExactConfiguration = toFlag True,
      Cabal.configBenchmarks         = toFlag (BenchStanzas `elem` pkgEnabledStanzas pkg),
      Cabal.configTests              = toFlag (TestStanzas  `elem` pkgEnabledStanzas pkg),
      Cabal.configPrograms           = progdb, --TODO: what does this actually get used for?
      Cabal.configPackageDBs         = Nothing : map Just pkgBuildPackageDBStack,

      --FIXME: hard coded:
      Cabal.configHcFlavor     = toFlag (compilerFlavor compiler),
      Cabal.configProgramPaths = [("ghc", "ghc-7.10.1")],
      Cabal.configOptimization = toFlag NoOptimisation
    }

    buildCommand'    = Cabal.buildCommand defaultProgramConfiguration
    buildFlags   _   = Cabal.emptyBuildFlags {
      Cabal.buildDistPref  = toFlag builddir,
      Cabal.buildVerbosity = toFlag verbosity
    }

    generateInstalledPackageInfo :: IO (Maybe InstalledPackageInfo)
    generateInstalledPackageInfo
      | not shouldRegister = return Nothing
      | otherwise = do
      absTmpDir <- canonicalizePath distTempDirectory --since we change dir
      withTempFile absTmpDir "package-registration-" $ \pkgConfFile hnd -> do
        hClose hnd
        let registerFlags _ = Cabal.emptyRegisterFlags {
              Cabal.regDistPref   = toFlag builddir,
              Cabal.regVerbosity  = toFlag verbosity,
              Cabal.regGenPkgConf = toFlag (Just pkgConfFile)
            }
        setup Cabal.registerCommand registerFlags

        (warns, ipkg) <- withUTF8FileContents pkgConfFile $ \pkgConfStr ->
          case Installed.parseInstalledPackageInfo pkgConfStr of
            Installed.ParseFailed perror -> pkgConfParseFailed perror
            Installed.ParseOk warns ipkg -> return (warns, ipkg)

        unless (null warns) $
          warn verbosity $ unlines (map (showPWarning pkgConfFile) warns)

        return $ Just ipkg {
            -- Important: we decide what the installed package id is, not
            -- the build system.
            Installed.installedPackageId = ipkgid
            --TODO: re-exported modules can also mention this ipkgid, and we'd need to update that too
          }

      where
        shouldRegister = Cabal.hasLibs pkgdesc
        pkgConfParseFailed :: Installed.PError -> IO a
        pkgConfParseFailed perror =
          die $ "Couldn't parse the output of 'setup register --gen-pkg-config':"
                ++ show perror

{-
  where
    pkgid            = packageId pkg
    buildCommand'    = buildCommand defaultProgramConfiguration
    buildFlags   _   = emptyBuildFlags {
      buildDistPref  = configDistPref configFlags,
      buildVerbosity = toFlag verbosity'
    }
    shouldHaddock    = fromFlag (installDocumentation installFlags)
    haddockFlags' _   = haddockFlags {
      haddockVerbosity = toFlag verbosity',
      haddockDistPref  = configDistPref configFlags
    }
    testsEnabled = fromFlag (configTests configFlags)
                   && fromFlagOrDefault False (installRunTests installFlags)
    testFlags _ = Cabal.emptyTestFlags {
      Cabal.testDistPref = configDistPref configFlags
    }
-}
    copyFlags _ = Cabal.emptyCopyFlags {
      Cabal.copyDistPref   = toFlag builddir,
      Cabal.copyDest       = toFlag InstallDirs.NoCopyDest,
      Cabal.copyVerbosity  = toFlag verbosity
    }

    scriptOptions = SetupScriptOptions {
        useCabalVersion   = anyVersion
      , useCompiler       = Just compiler
      , usePlatform       = Just platform
      , usePackageDB      = pkgSetupPackageDBStack
      , usePackageIndex   = Nothing
      , useProgramConfig  = progdb
      , useDistPref       = builddir
      , useLoggingHandle  = Nothing
      , useWorkingDir     = Just srcdir --TODO: make the dist dir relative to this
      , setupCacheLock    = Nothing
      , useWin32CleanHack = False
      , forceExternalSetupMethod = False
        -- If we have explicit setup dependencies, list them; otherwise, we give
        -- the empty list of dependencies; ideally, we would fix the version of
        -- Cabal here, so that we no longer need the special case for that in
        -- `compileSetupExecutable` in `externalSetupMethod`, but we don't yet
        -- know the version of Cabal at this point, but only find this there.
        -- Therefore, for now, we just leave this blank.
      , useDependencies          = []
      , useDependenciesExclusive = False
      }

{-
    shouldRegister = PackageDescription.hasLibs pkg
    registerFlags _ = Cabal.emptyRegisterFlags {
      Cabal.regDistPref   = configDistPref configFlags,
      Cabal.regVerbosity  = toFlag verbosity'
    }
    verbosity' = maybe verbosity snd useLogFile
    tempTemplate name = name ++ "-" ++ display pkgid
-}
    addDefaultInstallDirs :: Cabal.ConfigFlags -> IO Cabal.ConfigFlags
    addDefaultInstallDirs configFlags' = do
      defInstallDirs <- InstallDirs.defaultInstallDirs (compilerFlavor compiler) True False
      return $ configFlags' {
          Cabal.configInstallDirs =
            fmap toFlag $
            InstallDirs.substituteInstallDirTemplates env defInstallDirs {
              InstallDirs.prefix = InstallDirs.toPathTemplate $
                                     InstallDirs.fromPathTemplate (InstallDirs.prefix defInstallDirs)
                                     </> "store" --TODO: use the CabalDirLayout
                                     </> display ipkgid
            }
          }
        where
          env         = InstallDirs.initialPathTemplateEnv pkgid libname cinfo platform
          cinfo       = compilerInfo compiler
          libname     = LibraryName (display pkgid)
{-
    maybeGenPkgConf :: Maybe FilePath
                    -> IO (Maybe Installed.InstalledPackageInfo)
    maybeGenPkgConf mLogPath =
      if shouldRegister then do
        tmp <- getTemporaryDirectory
        withTempFile tmp (tempTemplate "pkgConf") $ \pkgConfFile handle -> do
          hClose handle
          let registerFlags' version = (registerFlags version) {
                Cabal.regGenPkgConf = toFlag (Just pkgConfFile)
              }
          setup Cabal.registerCommand registerFlags' mLogPath
          withUTF8FileContents pkgConfFile $ \pkgConfText ->
            case Installed.parseInstalledPackageInfo pkgConfText of
              Installed.ParseFailed perror    -> pkgConfParseFailed perror
              Installed.ParseOk warns pkgConf -> do
                unless (null warns) $
                  warn verbosity $ unlines (map (showPWarning pkgConfFile) warns)
                return (Just pkgConf)
      else return Nothing

    pkgConfParseFailed :: Installed.PError -> IO a
    pkgConfParseFailed perror =
      die $ "Couldn't parse the output of 'setup register --gen-pkg-config':"
            ++ show perror

    maybeLogPath :: IO (Maybe FilePath)
    maybeLogPath =
      case useLogFile of
         Nothing                 -> return Nothing
         Just (mkLogFileName, _) -> do
           let logFileName = mkLogFileName (packageId pkg) pkg_key
               logDir      = takeDirectory logFileName
           unless (null logDir) $ createDirectoryIfMissing True logDir
           logFileExists <- doesFileExist logFileName
           when logFileExists $ removeFile logFileName
           return (Just logFileName)
-}

--    setup :: CommandUI flags -> (Version -> flags) -> IO ()
    setup cmd flags =
      setupWrapper verbosity
                   scriptOptions
                   (Just pkgdesc)
                   cmd flags []
{-
    reexec cmd = do
      -- look for our own executable file and re-exec ourselves using a helper
      -- program like sudo to elevate privileges:
      self <- getExecutablePath
      weExist <- doesFileExist self
      if weExist
        then inDir workingDir $
               rawSystemExit verbosity cmd
                 [self, "install", "--only"
                 ,"--verbose=" ++ showForCabal verbosity]
        else die $ "Unable to find cabal executable at: " ++ self
-}


buildInplaceUnpackedPackage :: Verbosity -> Lock -> Lock
                            -> ElaboratedSharedConfig
                            -> GenericReadyPackage ElaboratedConfiguredPackage
                                                   InstalledPackageInfo
                            -> FilePath -> FilePath
                            -> IO (BuildResult' InstalledPackageInfo BuildSuccess BuildFailure)
buildInplaceUnpackedPackage verbosity _installLock _cacheLock
                            ElaboratedSharedConfig {
                              pkgConfigPlatform  = platform,
                              pkgConfigCompiler  = compiler,
                              pkgConfigProgramDb = progdb
                            }
                            rpkg@(ReadyPackage pkg deps) srcdir builddir = do

    putStrLn $ "buildInplaceUnpackedPackage: " ++ srcdir ++ " " ++ builddir

    --TODO: compare if this package's config has changed, 
    --      so have to load previous and compare 
    --TODO: check the file status cache to see if it's needed
    --TODO: if it built successfully then update the file status cache,
    --      both the files and the package config

    configChanged <- checkValueChanged (distUnpackedSrcConfigCache pkgid) rpkg
    print ("=============================== configChanged", configChanged)

    srcFilesChanged <- checkFileStatusChanged
                         srcdir (distUnpackedSrcFileStatusCache pkgid)
    print ("=============================== srcFilesChanged", srcFilesChanged)

    case (configChanged, srcFilesChanged) of
      (Unchanged (), False) -> do
        --TODO: reuse previous InstalledPackageInfo, e.g. stash in plan
        let ipkgid = InstalledPackageId (display pkgid ++ "-inplace")
        mipkg <- generateInstalledPackageInfo ipkgid
        
        let docsResult  = DocsNotTried
            testsResult = TestsNotTried
        return (Right (mipkg, BuildOk docsResult testsResult mipkg))

      _ -> do
        createDirectoryIfMissingVerbose verbosity False builddir

        -- Configure phase
        notice verbosity $ "Configuring " ++ display pkgid ++ "..."
        --TODO: think about these install dirs, we're not planning to install, but
        -- where should we configure them for?
        setup configureCommand' (\_ -> configureFlags)

        -- Build phase
        notice verbosity $ "Building " ++ display pkgid ++ "..."
        setup buildCommand' buildFlags

        -- Register locally
        --TODO: only need to re-register if input package config has changed
        --      (or if not registered in the first place)
        let ipkgid = InstalledPackageId (display pkgid ++ "-inplace")
        mipkg <- generateInstalledPackageInfo ipkgid
        case mipkg of
          Nothing   -> return ()
          Just ipkg -> do
            -- We register ourselves rather than via Setup.hs, because it's a bit
            -- cleaner (Setup.hs isn't allowed to expect to be able to modify the
            -- target system during register, it must be able to make a reg file).

            HcPkg.reregister (GHC.hcPkgInfo progdb) verbosity
                             (pkgRegisterPackageDBStack pkg)
                             (Right ipkg)

            --TODO: do this in a compiler agnostic way via Cabal lib
            --setup Cabal.registerCommand registerFlags

        updateFileStatusCache srcdir (distUnpackedSrcFileStatusCache pkgid)
          =<< getDirectoryContentsRecursive srcdir
        --TODO: does checkValueChanged always go with updateValueChangeCache?
        updateValueChangeCache (distUnpackedSrcConfigCache pkgid) rpkg ()

        let docsResult  = DocsNotTried
            testsResult = TestsNotTried
        return (Right (mipkg, BuildOk docsResult testsResult mipkg))

  where
    pkgid = packageId rpkg

    --FIXME: this is ridiculous, it's only needed for the setupWrapper, and
    -- it only needs a couple little bits from it
    pkgdesc = case Cabal.finalizePackageDescription
                     (pkgFlagAssignment pkg)
                     (const True)
                     platform (compilerInfo compiler) []
                     (enableStanzas (pkgEnabledStanzas pkg)
                                    (pkgDescription pkg))
                of Left _ -> error "finalizePackageDescription ReadyPackage failed"
                   Right (desc, _) -> desc

    configureCommand'= Cabal.configureCommand defaultProgramConfiguration
    --TODO: proper set of config options, not just Setup.hs interface flags
    --      or, get passed proper package config and translate into Setup.hs
    --      interface flags (or intermediate, and only translate if we go via
    --      the CLI)
    configureFlags   = Cabal.emptyConfigFlags {
      Cabal.configDistPref  = toFlag builddir,
      Cabal.configVerbosity = toFlag verbosity,

      Cabal.configConfigurationsFlags = pkgFlagAssignment pkg,
      -- We generate the legacy constraints as well as the new style precise deps.
      -- In the end only one set gets passed to Setup.hs configure, depending on
      -- the Cabal version we are talking to.
      Cabal.configConstraints  = [ thisPackageVersion (packageId deppkg)
                                 | deppkg <- ComponentDeps.nonSetupDeps deps ],
      Cabal.configDependencies = [ (packageName (Installed.sourcePackageId deppkg),
                                    Installed.installedPackageId deppkg)
                                 | deppkg <- ComponentDeps.nonSetupDeps deps ],
      -- Use '--exact-configuration' if supported.
      Cabal.configExactConfiguration = toFlag True,
      Cabal.configBenchmarks         = toFlag (BenchStanzas `elem` pkgEnabledStanzas pkg),
      Cabal.configTests              = toFlag (TestStanzas  `elem` pkgEnabledStanzas pkg),
      Cabal.configPrograms           = progdb, --TODO: what does this actually get used for?
      Cabal.configPackageDBs         = Nothing : map Just (pkgRegisterPackageDBStack pkg),

      --FIXME: hard coded:
      Cabal.configHcFlavor     = toFlag (compilerFlavor compiler),
      Cabal.configProgramPaths = [("ghc", "ghc-7.10.1")],
      Cabal.configOptimization = toFlag NoOptimisation
    }

    buildCommand'    = Cabal.buildCommand defaultProgramConfiguration
    buildFlags   _   = Cabal.emptyBuildFlags {
      Cabal.buildDistPref  = toFlag builddir,
      Cabal.buildVerbosity = toFlag verbosity
    }

    scriptOptions = SetupScriptOptions {
        useCabalVersion   = anyVersion
      , useCompiler       = Just compiler
      , usePlatform       = Just platform
      , usePackageDB      = pkgRegisterPackageDBStack pkg
      , usePackageIndex   = Nothing
      , useProgramConfig  = progdb
      , useDistPref       = builddir
      , useLoggingHandle  = Nothing
      , useWorkingDir     = Just srcdir --TODO: make the dist dir relative to this
      , setupCacheLock    = Nothing
      , useWin32CleanHack = False
      , forceExternalSetupMethod = False
        -- If we have explicit setup dependencies, list them; otherwise, we give
        -- the empty list of dependencies; ideally, we would fix the version of
        -- Cabal here, so that we no longer need the special case for that in
        -- `compileSetupExecutable` in `externalSetupMethod`, but we don't yet
        -- know the version of Cabal at this point, but only find this there.
        -- Therefore, for now, we just leave this blank.
      , useDependencies          = []
      , useDependenciesExclusive = False
      }

    setup cmd flags =
      setupWrapper verbosity
                   scriptOptions
                   (Just pkgdesc)
                   cmd flags []

    generateInstalledPackageInfo :: InstalledPackageId
                                 -> IO (Maybe InstalledPackageInfo)
    generateInstalledPackageInfo ipkgid
      | not shouldRegister = return Nothing
      | otherwise = do
      absTmpDir <- canonicalizePath distTempDirectory --since we change dir
      withTempFile absTmpDir "package-registration-" $ \pkgConfFile hnd -> do
        hClose hnd
        let registerFlags _ = Cabal.emptyRegisterFlags {
              Cabal.regDistPref   = toFlag builddir,
              Cabal.regVerbosity  = toFlag verbosity,
              Cabal.regGenPkgConf = toFlag (Just pkgConfFile),
              Cabal.regInPlace    = toFlag True
            }
        setup Cabal.registerCommand registerFlags

        (warns, ipkg) <- withUTF8FileContents pkgConfFile $ \pkgConfStr ->
          case Installed.parseInstalledPackageInfo pkgConfStr of
            Installed.ParseFailed perror -> pkgConfParseFailed perror
            Installed.ParseOk warns ipkg -> return (warns, ipkg)

        unless (null warns) $
          warn verbosity $ unlines (map (showPWarning pkgConfFile) warns)

        return $ Just ipkg {
            -- Important: we decide what the installed package id is, not
            -- the build system.
            Installed.installedPackageId = ipkgid
          }

      where
        shouldRegister = Cabal.hasLibs pkgdesc
        pkgConfParseFailed :: Installed.PError -> IO a
        pkgConfParseFailed perror =
          die $ "Couldn't parse the output of 'setup register --gen-pkg-config':"
                ++ show perror

{-
printPlan :: Verbosity -> [ReadyPackage] -> IO ()
printPlan verbosity pkgs = do
    notice verbosity $ unlines $
        ("In order, the following would be installed:")
      : map showPkgAndReason pkgs
  where

    showPkgAndReason pkg = display (packageId pkg)
                        ++ showStanzas (stanzas pkg)
                        ++ " " ++ showBuildStyle pkg


    stanzas :: ReadyPackage -> [OptionalStanza]
    stanzas (ReadyPackage _ _ sts _ _) = sts

    showStanzas :: [OptionalStanza] -> String
    showStanzas = concatMap ((' ' :) . showStanza)
    showStanza TestStanzas  = "*test"
    showStanza BenchStanzas = "*bench"

    showBuildStyle (ReadyPackage _ _ _ _ buildstyle) = show buildstyle


linearizeInstallPlan :: InstallPlan -> [ReadyPackage]
linearizeInstallPlan plan =
    unfoldr next plan
  where
    next plan' = case InstallPlan.ready plan' of
      []      -> Nothing
      (pkg:_) -> Just (pkg, plan'')
        where
          pkgid  = installedPackageId pkg
          plan'' = InstallPlan.completed pkgid
                     (BuildOk DocsNotTried TestsNotTried
                              (Just $ Installed.emptyInstalledPackageInfo
                              { Installed.sourcePackageId = packageId pkg
                              , Installed.installedPackageId = pkgid }))
                     (InstallPlan.processing [pkg] plan')
          --FIXME: This is a bit of a hack,
          -- pretending that each package is installed
          -- It's doubly a hack because the installed package ID
          -- didn't get updated...
-}

printPlan :: GenericInstallPlan ipkg ElaboratedConfiguredPackage iresult ifailure -> String
printPlan installPlan =
      "These packages are using local inplace builds: " ++
      intercalate ", "
        [ display (packageId pkg)
        | InstallPlan.Configured pkg@ElaboratedConfiguredPackage { pkgBuildStyle = BuildInplaceOnly }
            <- InstallPlan.toList installPlan
        ]

       

-------------------------------
-- Calculating package hashes
--

-- The basic idea is simple, hash the combination of:
--
--   * the package tarball
--   * the ids of all the direct dependencies
--   * other local configuration (flags, profiling, etc)

-- | Calculate a 'InstalledPackageId' for a package using our nix-style
-- inputs hashing method.
--
hashedInstalledPackageId :: PackageHashInputs -> InstalledPackageId
hashedInstalledPackageId pkghashinputs@PackageHashInputs{pkgHashPkgId} =
    InstalledPackageId $
         display pkgHashPkgId   -- to be a bit user friendly
      ++ "-"
      ++ showHashValue (hashedInstalledPackageIdHash pkghashinputs)

-- | All the information that contribues to a package's hash, and thus its
-- 'InstalledPackageIds'.
--
data PackageHashInputs = PackageHashInputs {
       pkgHashPkgId         :: PackageId,
       pkgHashSourceHash    :: PackageSourceHash,
       pkgHashDirectDeps    :: [InstalledPackageId],
       pkgHashOtherConfig   :: PackageHashConfigInputs
     }

type PackageSourceHash = HashValue

-- | Those parts of the package configuration that contribute to the
-- package hash.
--
data PackageHashConfigInputs = PackageHashConfigInputs {
--     pkgHashCompilerId              :: CompilerId,
--     pkgHashPlatform                :: Platform,
--     pkgHashFlagAssignment          :: FlagAssignment, -- complete not partial
--     pkgHashConfigureOptions        :: [String], -- just ./configure for build-type Configure
--     pkgHashOptimisationLevel       :: OptimisationLevel,
--     pkgHashLibraryShared           :: Bool,
       pkgHashLibraryProfiling        :: Bool
--     pkgHashLibraryProfilingLevel   :: ProfDetailLevel,
--     pkgHashHpcEnabled              :: Bool,
--     pkgHashToolsVersions     ?
--     pkgHashToolsExtraOptions ?
-- and what about: docs, split objs, ghci-lib, exe striping? prog prefix/suffix
     }
  deriving Show

-- | Calculate the overall hash to be used for an 'InstalledPackageId'
--
hashedInstalledPackageIdHash :: PackageHashInputs -> HashValue
hashedInstalledPackageIdHash PackageHashInputs{..} =
    hashValue $
      -- this is a bit ad-hoc and not exactly fast, but should be ok
      (LBS.Char8.pack . concat) $
         display pkgHashPkgId    -- strictly speaking this is redundant
       : showHashValue pkgHashSourceHash
       : show pkgHashOtherConfig
       : map display (sort pkgHashDirectDeps)


-----------------------------------------------
-- The specific choice of hash implementation
--

-- Is a crypto hash necessary here? One thing to consider is who controls the
-- inputs and what's the result of a hash collision. Obviously we should not
-- install packages we don't trust because they can run all sorts of code, but
-- if I've checked there's no TH, no custom Setup etc, is there still a
-- problem? If someone provided us a tarball that hashed to the same value as
-- some other package and we installed it, we could end up re-using that
-- installed package in place of another one we wanted. So yes, in general
-- there is some value in preventing intentional hash collisions in installed
-- package ids.

newtype HashValue = HashValue (SHA.Digest SHA.SHA1State)
  deriving Show

hashValue :: LBS.ByteString -> HashValue
hashValue = HashValue . SHA.sha1

showHashValue :: HashValue -> String
showHashValue (HashValue digest) = SHA.showDigest digest

readFileHashValue :: FilePath -> IO HashValue
readFileHashValue tarball =
    withBinaryFile tarball ReadMode $ \hnd ->
      evaluate . hashValue =<< LBS.hGetContents hnd


{-
makeRelativeFully :: FilePath -> FilePath -> IO FilePath
makeRelativeFully base target = do
    cbase   <- canonicalizePath base 
    ctarget <- canonicalizePath target
    go (splitPath cbase)
       (splitPath ctarget)
  where
    go (b:bs) (t:ts) | b == t = go bs ts
    go bs ts = return $ joinPath (replicate (length bs) ".." ++ ts)
-}

