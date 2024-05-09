{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Build
-- Copyright   :  Isaac Jones 2003-2005,
--                Ross Paterson 2006,
--                Duncan Coutts 2007-2008, 2012
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point to actually building the modules in a package. It
-- doesn't actually do much itself, most of the work is delegated to
-- compiler-specific actions. It does do some non-compiler specific bits like
-- running pre-processors.
module Distribution.Simple.Build
  ( -- * Build
    build
  , build_setupHooks
  , buildComponent
  , runPostBuildHooks

    -- * Repl
  , repl
  , repl_setupHooks
  , startInterpreter

    -- * Build preparation
  , preBuildComponent
  , runPreBuildHooks
  , AutogenFile (..)
  , AutogenFileContents
  , writeBuiltinAutogenFiles
  , writeAutogenFiles

    -- ** Legacy functions
  , componentInitialBuildSteps
  , initialBuildSteps

    -- * Internal package database creation
  , createInternalPackageDB

    -- * Handling of internal build tools
  , addInternalBuildTools
  ) where

import Distribution.Compat.Prelude
import Distribution.Utils.Generic
import Prelude ()

import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.Dependency
import Distribution.Types.ExecutableScope
import Distribution.Types.ForeignLib
import Distribution.Types.LibraryVisibility
import Distribution.Types.LocalBuildInfo
import Distribution.Types.ModuleRenaming
import Distribution.Types.MungedPackageId
import Distribution.Types.MungedPackageName
import Distribution.Types.ParStrat
import Distribution.Types.TargetInfo
import Distribution.Utils.Path

import Distribution.Backpack
import Distribution.Backpack.DescribeUnitId
import Distribution.Package
import qualified Distribution.Simple.GHC as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.HaskellSuite as HaskellSuite
import qualified Distribution.Simple.PackageIndex as Index
import qualified Distribution.Simple.UHC as UHC

import Distribution.Simple.Build.Macros (generateCabalMacrosHeader)
import Distribution.Simple.Build.PackageInfoModule (generatePackageInfoModule)
import Distribution.Simple.Build.PathsModule (generatePathsModule, pkgPathEnvVar)
import qualified Distribution.Simple.Program.HcPkg as HcPkg

import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription
import Distribution.Simple.Compiler

import Distribution.Simple.BuildPaths
import Distribution.Simple.BuildTarget
import Distribution.Simple.BuildToolDepends
import Distribution.Simple.Configure
import Distribution.Simple.Errors
import Distribution.Simple.Flag
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Program.Builtin (haskellSuiteProgram)
import Distribution.Simple.Program.Db
import qualified Distribution.Simple.Program.GHC as GHC
import Distribution.Simple.Program.Types
import Distribution.Simple.Register
import Distribution.Simple.Setup.Build
import Distribution.Simple.Setup.Common
import Distribution.Simple.Setup.Config
import Distribution.Simple.Setup.Repl
import Distribution.Simple.SetupHooks.Internal
  ( BuildingWhat (..)
  , buildingWhatVerbosity
  )
import qualified Distribution.Simple.SetupHooks.Internal as SetupHooks
import qualified Distribution.Simple.SetupHooks.Rule as SetupHooks
import Distribution.Simple.ShowBuildInfo
import Distribution.Simple.Test.LibV09
import Distribution.Simple.Utils
import Distribution.Utils.Json
import Distribution.Utils.ShortText (ShortText, fromShortText, toShortText)

import Distribution.Pretty
import Distribution.System
import Distribution.Verbosity
import Distribution.Version (thisVersion)

import Distribution.Compat.Graph (IsNode (..))

import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import System.Directory (doesFileExist, removeFile)
import System.FilePath (takeDirectory)

-- -----------------------------------------------------------------------------

-- | Build the libraries and executables in this package.
build
  :: PackageDescription
  -- ^ Mostly information from the .cabal file
  -> LocalBuildInfo
  -- ^ Configuration information
  -> BuildFlags
  -- ^ Flags that the user passed to build
  -> [PPSuffixHandler]
  -- ^ preprocessors to run before compiling
  -> IO ()
build pkg lbi flags suffixHandlers =
  void $ build_setupHooks noHooks pkg lbi flags suffixHandlers
  where
    noHooks = (const $ return [], const $ return ())

build_setupHooks
  :: ( SetupHooks.PreBuildComponentInputs -> IO [SetupHooks.MonitorFilePath]
     , SetupHooks.PostBuildComponentInputs -> IO ()
     )
  -- ^ build hooks
  -> PackageDescription
  -- ^ Mostly information from the .cabal file
  -> LocalBuildInfo
  -- ^ Configuration information
  -> BuildFlags
  -- ^ Flags that the user passed to build
  -> [PPSuffixHandler]
  -- ^ preprocessors to run before compiling
  -> IO [SetupHooks.MonitorFilePath]
build_setupHooks
  (preBuildHook, postBuildHook)
  pkg_descr
  lbi
  flags
  suffixHandlers = do
    let verbosity = fromFlag $ buildVerbosity flags
        distPref = fromFlag $ buildDistPref flags
    checkSemaphoreSupport verbosity (compiler lbi) flags
    targets <- readTargetInfos verbosity pkg_descr lbi (buildTargets flags)
    let componentsToBuild = neededTargetsInBuildOrder' pkg_descr lbi (map nodeKey targets)
    info verbosity $
      "Component build order: "
        ++ intercalate
          ", "
          ( map
              (showComponentName . componentLocalName . targetCLBI)
              componentsToBuild
          )

    when (null targets) $
      -- Only bother with this message if we're building the whole package
      setupMessage verbosity "Building" (packageId pkg_descr)

    internalPackageDB <- createInternalPackageDB verbosity lbi distPref

    -- Before the actual building, dump out build-information.
    -- This way, if the actual compilation failed, the options have still been
    -- dumped.
    dumpBuildInfo verbosity distPref (configDumpBuildInfo (configFlags lbi)) pkg_descr lbi flags

    -- Now do the actual building
    (mons, _) <- (\f -> foldM f ([], installedPkgs lbi) componentsToBuild) $ \(monsAcc, index) target -> do
      let comp = targetComponent target
          clbi = targetCLBI target
          bi = componentBuildInfo comp
          -- Include any build-tool-depends on build tools internal to the current package.
          progs' = addInternalBuildTools pkg_descr lbi bi (withPrograms lbi)
          lbi' =
            lbi
              { withPrograms = progs'
              , withPackageDB = withPackageDB lbi ++ [internalPackageDB]
              , installedPkgs = index
              }
          pbci = SetupHooks.PreBuildComponentInputs (BuildNormal flags) lbi' target
      mons <- preBuildComponent (preBuildHook pbci) verbosity lbi' target
      let numJobs = buildNumJobs flags
      par_strat <-
        toFlag <$> case buildUseSemaphore flags of
          Flag sem_name -> case numJobs of
            Flag{} -> do
              warn verbosity $ "Ignoring -j due to --semaphore"
              return $ UseSem sem_name
            NoFlag -> return $ UseSem sem_name
          NoFlag -> return $ case numJobs of
            Flag n -> NumJobs n
            NoFlag -> Serial
      mb_ipi <-
        buildComponent
          flags
          par_strat
          pkg_descr
          lbi'
          suffixHandlers
          comp
          clbi
          distPref
      let postBuildInputs =
            SetupHooks.PostBuildComponentInputs
              { SetupHooks.buildFlags = flags
              , SetupHooks.localBuildInfo = lbi'
              , SetupHooks.targetInfo = target
              }
      postBuildHook postBuildInputs
      return (monsAcc ++ mons, maybe index (Index.insert `flip` index) mb_ipi)
    return mons

runPreBuildHooks
  :: SetupHooks.PreBuildComponentInputs
  -> SetupHooks.Rules SetupHooks.PreBuildComponentInputs
  -> IO [SetupHooks.MonitorFilePath]
runPreBuildHooks
  pbci@SetupHooks.PreBuildComponentInputs
    { SetupHooks.buildingWhat = what
    , SetupHooks.localBuildInfo = lbi
    , SetupHooks.targetInfo = tgt
    }
  pbRules = do
    let verbosity = buildingWhatVerbosity what
    (rules, monitors) <- SetupHooks.computeRules verbosity pbci pbRules
    SetupHooks.executeRules verbosity lbi tgt rules
    return monitors

runPostBuildHooks
  :: BuildFlags
  -> LocalBuildInfo
  -> TargetInfo
  -> (SetupHooks.PostBuildComponentInputs -> IO ())
  -> IO ()
runPostBuildHooks flags lbi tgt postBuild =
  let inputs =
        SetupHooks.PostBuildComponentInputs
          { SetupHooks.buildFlags = flags
          , SetupHooks.localBuildInfo = lbi
          , SetupHooks.targetInfo = tgt
          }
   in postBuild inputs

-- | Check for conditions that would prevent the build from succeeding.
checkSemaphoreSupport
  :: Verbosity -> Compiler -> BuildFlags -> IO ()
checkSemaphoreSupport verbosity comp flags = do
  unless (jsemSupported comp || (isNothing (flagToMaybe (buildUseSemaphore flags)))) $
    dieWithException verbosity CheckSemaphoreSupport

-- | Write available build information for 'LocalBuildInfo' to disk.
--
-- Dumps detailed build information 'build-info.json' to the given directory.
-- Build information contains basics such as compiler details, but also
-- lists what modules a component contains and how to compile the component, assuming
-- lib:Cabal made sure that dependencies are up-to-date.
dumpBuildInfo
  :: Verbosity
  -> SymbolicPath Pkg (Dir Dist)
  -- ^ To which directory should the build-info be dumped?
  -> Flag DumpBuildInfo
  -- ^ Should we dump detailed build information for this component?
  -> PackageDescription
  -- ^ Mostly information from the .cabal file
  -> LocalBuildInfo
  -- ^ Configuration information
  -> BuildFlags
  -- ^ Flags that the user passed to build
  -> IO ()
dumpBuildInfo verbosity distPref dumpBuildInfoFlag pkg_descr lbi flags = do
  let mbWorkDir = flagToMaybe $ buildWorkingDir flags
  when shouldDumpBuildInfo $ do
    -- Changing this line might break consumers of the dumped build info.
    -- Announce changes on mailing lists!
    let activeTargets = allTargetsInBuildOrder' pkg_descr lbi
    info verbosity $
      "Dump build information for: "
        ++ intercalate
          ", "
          ( map
              (showComponentName . componentLocalName . targetCLBI)
              activeTargets
          )

    wdir <- absoluteWorkingDir mbWorkDir

    (compilerProg, _) <- case flavorToProgram (compilerFlavor (compiler lbi)) of
      Nothing ->
        dieWithException verbosity $ UnknownCompilerFlavor (compilerFlavor (compiler lbi))
      Just program -> requireProgram verbosity program (withPrograms lbi)

    let (warns, json) = mkBuildInfo wdir pkg_descr lbi flags (compilerProg, compiler lbi) activeTargets
        buildInfoText = renderJson json
    unless (null warns) $
      warn verbosity $
        "Encountered warnings while dumping build-info:\n"
          ++ unlines warns
    LBS.writeFile buildInfoFile buildInfoText

  when (not shouldDumpBuildInfo) $ do
    -- Remove existing build-info.json as it might be outdated now.
    exists <- doesFileExist buildInfoFile
    when exists $ removeFile buildInfoFile
  where
    buildInfoFile = interpretSymbolicPathLBI lbi $ buildInfoPref distPref
    shouldDumpBuildInfo = fromFlagOrDefault NoDumpBuildInfo dumpBuildInfoFlag == DumpBuildInfo

    -- \| Given the flavor of the compiler, try to find out
    -- which program we need.
    flavorToProgram :: CompilerFlavor -> Maybe Program
    flavorToProgram GHC = Just ghcProgram
    flavorToProgram GHCJS = Just ghcjsProgram
    flavorToProgram UHC = Just uhcProgram
    flavorToProgram JHC = Just jhcProgram
    flavorToProgram HaskellSuite{} = Just haskellSuiteProgram
    flavorToProgram _ = Nothing

repl
  :: PackageDescription
  -- ^ Mostly information from the .cabal file
  -> LocalBuildInfo
  -- ^ Configuration information
  -> ReplFlags
  -- ^ Flags that the user passed to build
  -> [PPSuffixHandler]
  -- ^ preprocessors to run before compiling
  -> [String]
  -> IO ()
repl = repl_setupHooks (const $ return [])

repl_setupHooks
  :: (SetupHooks.PreBuildComponentInputs -> IO [SetupHooks.MonitorFilePath])
  -- ^ pre-build hook
  -> PackageDescription
  -- ^ Mostly information from the .cabal file
  -> LocalBuildInfo
  -- ^ Configuration information
  -> ReplFlags
  -- ^ Flags that the user passed to build
  -> [PPSuffixHandler]
  -- ^ preprocessors to run before compiling
  -> [String]
  -> IO ()
repl_setupHooks
  preBuildHook
  pkg_descr
  lbi
  flags
  suffixHandlers
  args = do
    let distPref = fromFlag (replDistPref flags)
        verbosity = fromFlag (replVerbosity flags)

    target <-
      readTargetInfos verbosity pkg_descr lbi args >>= \r -> case r of
        -- This seems DEEPLY questionable.
        [] -> case allTargetsInBuildOrder' pkg_descr lbi of
          (target : _) -> return target
          [] -> dieWithException verbosity $ FailedToDetermineTarget
        [target] -> return target
        _ -> dieWithException verbosity $ NoMultipleTargets
    let componentsToBuild = neededTargetsInBuildOrder' pkg_descr lbi [nodeKey target]
    debug verbosity $
      "Component build order: "
        ++ intercalate
          ", "
          ( map
              (showComponentName . componentLocalName . targetCLBI)
              componentsToBuild
          )

    internalPackageDB <- createInternalPackageDB verbosity lbi distPref

    let lbiForComponent comp lbi' =
          lbi'
            { withPackageDB = withPackageDB lbi ++ [internalPackageDB]
            , withPrograms =
                -- Include any build-tool-depends on build tools internal to the current package.
                addInternalBuildTools
                  pkg_descr
                  lbi'
                  (componentBuildInfo comp)
                  (withPrograms lbi')
            }
        pbci lbi' tgt = SetupHooks.PreBuildComponentInputs (BuildRepl flags) lbi' tgt

    -- build any dependent components
    sequence_
      [ do
        let clbi = targetCLBI subtarget
            comp = targetComponent subtarget
            lbi' = lbiForComponent comp lbi
        _monitors <-
          preBuildComponent (preBuildHook (pbci lbi' subtarget)) verbosity lbi' subtarget
        buildComponent
          (mempty{buildCommonFlags = mempty{setupVerbosity = toFlag verbosity}})
          NoFlag
          pkg_descr
          lbi'
          suffixHandlers
          comp
          clbi
          distPref
      | subtarget <- safeInit componentsToBuild
      ]

    -- REPL for target components
    let clbi = targetCLBI target
        comp = targetComponent target
        lbi' = lbiForComponent comp lbi
    _monitors <-
      preBuildComponent (preBuildHook (pbci lbi' target)) verbosity lbi' target
    replComponent flags verbosity pkg_descr lbi' suffixHandlers comp clbi distPref

-- | Start an interpreter without loading any package files.
startInterpreter
  :: Verbosity
  -> ProgramDb
  -> Compiler
  -> Platform
  -> PackageDBStack
  -> IO ()
startInterpreter verbosity programDb comp platform packageDBs =
  case compilerFlavor comp of
    GHC -> GHC.startInterpreter verbosity programDb comp platform packageDBs
    GHCJS -> GHCJS.startInterpreter verbosity programDb comp platform packageDBs
    _ -> dieWithException verbosity REPLNotSupported

buildComponent
  :: BuildFlags
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> [PPSuffixHandler]
  -> Component
  -> ComponentLocalBuildInfo
  -> SymbolicPath Pkg (Dir Dist)
  -> IO (Maybe InstalledPackageInfo)
buildComponent flags _ _ _ _ (CTest TestSuite{testInterface = TestSuiteUnsupported tt}) _ _ =
  dieWithException (fromFlag $ buildVerbosity flags) $
    NoSupportBuildingTestSuite tt
buildComponent flags _ _ _ _ (CBench Benchmark{benchmarkInterface = BenchmarkUnsupported tt}) _ _ =
  dieWithException (fromFlag $ buildVerbosity flags) $
    NoSupportBuildingBenchMark tt
buildComponent
  flags
  numJobs
  pkg_descr
  lbi0
  suffixHandlers
  comp@( CTest
          test@TestSuite{testInterface = TestSuiteLibV09{}}
        )
  clbi -- This ComponentLocalBuildInfo corresponds to a detailed
  -- test suite and not a real component. It should not
  -- be used, except to construct the CLBIs for the
  -- library and stub executable that will actually be
  -- built.
  distPref =
    do
      inplaceDir <- absoluteWorkingDirLBI lbi0
      let verbosity = fromFlag $ buildVerbosity flags
      let (pkg, lib, libClbi, lbi, ipi, exe, exeClbi) =
            testSuiteLibV09AsLibAndExe pkg_descr test clbi lbi0 inplaceDir distPref
      preprocessComponent pkg_descr comp lbi clbi False verbosity suffixHandlers
      extras <- preprocessExtras verbosity comp lbi -- TODO find cpphs processed files
      (genDir, generatedExtras) <- generateCode (testCodeGenerators test) (testName test) pkg_descr (testBuildInfo test) lbi clbi verbosity
      setupMessage'
        verbosity
        "Building"
        (packageId pkg_descr)
        (componentLocalName clbi)
        (maybeComponentInstantiatedWith clbi)
      let libbi = libBuildInfo lib
          lib' = lib{libBuildInfo = addSrcDir (addExtraOtherModules libbi generatedExtras) genDir}
      buildLib flags numJobs pkg lbi lib' libClbi
      -- NB: need to enable multiple instances here, because on 7.10+
      -- the package name is the same as the library, and we still
      -- want the registration to go through.
      registerPackage
        verbosity
        (compiler lbi)
        (withPrograms lbi)
        (mbWorkDirLBI lbi)
        (withPackageDB lbi)
        ipi
        HcPkg.defaultRegisterOptions
          { HcPkg.registerMultiInstance = True
          }
      let ebi = buildInfo exe
          -- NB: The stub executable is linked against the test-library
          --     which already contains all `other-modules`, so we need
          --     to remove those from the stub-exe's build-info
          exe' = exe{buildInfo = (addExtraCSources ebi extras){otherModules = []}}
      buildExe verbosity numJobs pkg_descr lbi exe' exeClbi
      return Nothing -- Can't depend on test suite
buildComponent
  flags
  numJobs
  pkg_descr
  lbi
  suffixHandlers
  comp
  clbi
  distPref =
    do
      let verbosity = fromFlag $ buildVerbosity flags
      preprocessComponent pkg_descr comp lbi clbi False verbosity suffixHandlers
      extras <- preprocessExtras verbosity comp lbi
      setupMessage'
        verbosity
        "Building"
        (packageId pkg_descr)
        (componentLocalName clbi)
        (maybeComponentInstantiatedWith clbi)
      case comp of
        CLib lib -> do
          let libbi = libBuildInfo lib
              lib' =
                lib
                  { libBuildInfo =
                      flip addExtraAsmSources extras $
                        flip addExtraCmmSources extras $
                          flip addExtraCxxSources extras $
                            flip addExtraCSources extras $
                              flip addExtraJsSources extras $
                                libbi
                  }

          buildLib flags numJobs pkg_descr lbi lib' clbi

          let oneComponentRequested (OneComponentRequestedSpec _) = True
              oneComponentRequested _ = False
          -- Don't register inplace if we're only building a single component;
          -- it's not necessary because there won't be any subsequent builds
          -- that need to tag us
          if (not (oneComponentRequested (componentEnabledSpec lbi)))
            then do
              -- Register the library in-place, so exes can depend
              -- on internally defined libraries.
              inplaceDir <- absoluteWorkingDirLBI lbi
              let
                -- The in place registration uses the "-inplace" suffix, not an ABI hash
                installedPkgInfo =
                  inplaceInstalledPackageInfo
                    inplaceDir
                    distPref
                    pkg_descr
                    -- NB: Use a fake ABI hash to avoid
                    -- needing to recompute it every build.
                    (mkAbiHash "inplace")
                    lib'
                    lbi
                    clbi
              debug verbosity $ "Registering inplace:\n" ++ (IPI.showInstalledPackageInfo installedPkgInfo)
              registerPackage
                verbosity
                (compiler lbi)
                (withPrograms lbi)
                (flagToMaybe $ buildWorkingDir flags)
                (withPackageDB lbi)
                installedPkgInfo
                HcPkg.defaultRegisterOptions
                  { HcPkg.registerMultiInstance = True
                  }
              return (Just installedPkgInfo)
            else return Nothing
        CFLib flib -> do
          buildFLib verbosity numJobs pkg_descr lbi flib clbi
          return Nothing
        CExe exe -> do
          let ebi = buildInfo exe
              exe' = exe{buildInfo = addExtraCSources ebi extras}
          buildExe verbosity numJobs pkg_descr lbi exe' clbi
          return Nothing
        CTest test@TestSuite{testInterface = TestSuiteExeV10{}} -> do
          let exe = testSuiteExeV10AsExe test
          (genDir, generatedExtras) <- generateCode (testCodeGenerators test) (testName test) pkg_descr (testBuildInfo test) lbi clbi verbosity
          let ebi = buildInfo exe
              exe' = exe{buildInfo = addSrcDir (addExtraOtherModules (addExtraCSources ebi extras) generatedExtras) genDir} -- todo extend hssrcdirs
          buildExe verbosity numJobs pkg_descr lbi exe' clbi
          return Nothing
        CBench bm@Benchmark{benchmarkInterface = BenchmarkExeV10{}} -> do
          let exe = benchmarkExeV10asExe bm
          let ebi = buildInfo exe
              exe' = exe{buildInfo = addExtraCSources ebi extras}
          buildExe verbosity numJobs pkg_descr lbi exe' clbi
          return Nothing
#if __GLASGOW_HASKELL__ < 811
-- silence pattern-match warnings prior to GHC 9.0
        _ -> error "impossible"
#endif

generateCode
  :: [String]
  -> UnqualComponentName
  -> PackageDescription
  -> BuildInfo
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> Verbosity
  -> IO (SymbolicPath Pkg (Dir Source), [ModuleName.ModuleName])
generateCode codeGens nm pdesc bi lbi clbi verbosity = do
  when (not . null $ codeGens) $ createDirectoryIfMissingVerbose verbosity True $ i tgtDir
  (\x -> (tgtDir, x)) . concat <$> mapM go codeGens
  where
    allLibs = (maybe id (:) $ library pdesc) (subLibraries pdesc)
    dependencyLibs = filter (const True) allLibs -- intersect with componentPackageDeps of clbi
    srcDirs = concatMap (hsSourceDirs . libBuildInfo) dependencyLibs
    nm' = unUnqualComponentName nm
    mbWorkDir = mbWorkDirLBI lbi
    i = interpretSymbolicPath mbWorkDir -- See Note [Symbolic paths] in Distribution.Utils.Path
    tgtDir = buildDir lbi </> makeRelativePathEx (nm' </> nm' ++ "-gen")
    go :: String -> IO [ModuleName.ModuleName]
    go codeGenProg =
      fmap fromString . lines
        <$> getDbProgramOutputCwd
          verbosity
          mbWorkDir
          (simpleProgram codeGenProg)
          (withPrograms lbi)
          ( map interpretSymbolicPathCWD (tgtDir : srcDirs)
              ++ ( "--"
                    : GHC.renderGhcOptions (compiler lbi) (hostPlatform lbi) (GHC.componentGhcOptions verbosity lbi bi clbi tgtDir)
                 )
          )

-- | Add extra C sources generated by preprocessing to build
-- information.
addExtraCSources :: BuildInfo -> [SymbolicPath Pkg File] -> BuildInfo
addExtraCSources bi extras = bi{cSources = new}
  where
    new = ordNub (extras ++ cSources bi)

-- | Add extra C++ sources generated by preprocessing to build
-- information.
addExtraCxxSources :: BuildInfo -> [SymbolicPath Pkg File] -> BuildInfo
addExtraCxxSources bi extras = bi{cxxSources = new}
  where
    new = ordNub (extras ++ cxxSources bi)

-- | Add extra C-- sources generated by preprocessing to build
-- information.
addExtraCmmSources :: BuildInfo -> [SymbolicPath Pkg File] -> BuildInfo
addExtraCmmSources bi extras = bi{cmmSources = new}
  where
    new = ordNub (extras ++ cmmSources bi)

-- | Add extra ASM sources generated by preprocessing to build
-- information.
addExtraAsmSources :: BuildInfo -> [SymbolicPath Pkg File] -> BuildInfo
addExtraAsmSources bi extras = bi{asmSources = new}
  where
    new = ordNub (extras ++ asmSources bi)

-- | Add extra JS sources generated by preprocessing to build
-- information.
addExtraJsSources :: BuildInfo -> [SymbolicPath Pkg File] -> BuildInfo
addExtraJsSources bi extras = bi{jsSources = new}
  where
    new = ordNub (extras ++ jsSources bi)

-- | Add extra HS modules generated by preprocessing to build
-- information.
addExtraOtherModules :: BuildInfo -> [ModuleName.ModuleName] -> BuildInfo
addExtraOtherModules bi extras = bi{otherModules = new}
  where
    new = ordNub (extras ++ otherModules bi)

-- | Add extra source dir for generated modules.
addSrcDir :: BuildInfo -> SymbolicPath Pkg (Dir Source) -> BuildInfo
addSrcDir bi extra = bi{hsSourceDirs = new}
  where
    new = ordNub (extra : hsSourceDirs bi)

replComponent
  :: ReplFlags
  -> Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> [PPSuffixHandler]
  -> Component
  -> ComponentLocalBuildInfo
  -> SymbolicPath Pkg (Dir Dist)
  -> IO ()
replComponent _ verbosity _ _ _ (CTest TestSuite{testInterface = TestSuiteUnsupported tt}) _ _ =
  dieWithException verbosity $ NoSupportBuildingTestSuite tt
replComponent _ verbosity _ _ _ (CBench Benchmark{benchmarkInterface = BenchmarkUnsupported tt}) _ _ =
  dieWithException verbosity $ NoSupportBuildingBenchMark tt
replComponent
  replFlags
  verbosity
  pkg_descr
  lbi0
  suffixHandlers
  comp@( CTest
          test@TestSuite{testInterface = TestSuiteLibV09{}}
        )
  clbi
  distPref = do
    inplaceDir <- absoluteWorkingDirLBI lbi0
    let (pkg, lib, libClbi, lbi, _, _, _) =
          testSuiteLibV09AsLibAndExe pkg_descr test clbi lbi0 inplaceDir distPref
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixHandlers
    extras <- preprocessExtras verbosity comp lbi
    let libbi = libBuildInfo lib
        lib' = lib{libBuildInfo = libbi{cSources = cSources libbi ++ extras}}
    replLib replFlags pkg lbi lib' libClbi
replComponent
  replFlags
  verbosity
  pkg_descr
  lbi
  suffixHandlers
  comp
  clbi
  _ =
    do
      preprocessComponent pkg_descr comp lbi clbi False verbosity suffixHandlers
      extras <- preprocessExtras verbosity comp lbi
      case comp of
        CLib lib -> do
          let libbi = libBuildInfo lib
              lib' = lib{libBuildInfo = libbi{cSources = cSources libbi ++ extras}}
          replLib replFlags pkg_descr lbi lib' clbi
        CFLib flib ->
          replFLib replFlags pkg_descr lbi flib clbi
        CExe exe -> do
          let ebi = buildInfo exe
              exe' = exe{buildInfo = ebi{cSources = cSources ebi ++ extras}}
          replExe replFlags pkg_descr lbi exe' clbi
        CTest test@TestSuite{testInterface = TestSuiteExeV10{}} -> do
          let exe = testSuiteExeV10AsExe test
          let ebi = buildInfo exe
              exe' = exe{buildInfo = ebi{cSources = cSources ebi ++ extras}}
          replExe replFlags pkg_descr lbi exe' clbi
        CBench bm@Benchmark{benchmarkInterface = BenchmarkExeV10{}} -> do
          let exe = benchmarkExeV10asExe bm
          let ebi = buildInfo exe
              exe' = exe{buildInfo = ebi{cSources = cSources ebi ++ extras}}
          replExe replFlags pkg_descr lbi exe' clbi
#if __GLASGOW_HASKELL__ < 811
-- silence pattern-match warnings prior to GHC 9.0
        _ -> error "impossible"
#endif

----------------------------------------------------
-- Shared code for buildComponent and replComponent
--

-- | Translate a exe-style 'TestSuite' component into an exe for building
testSuiteExeV10AsExe :: TestSuite -> Executable
testSuiteExeV10AsExe test@TestSuite{testInterface = TestSuiteExeV10 _ mainFile} =
  Executable
    { exeName = testName test
    , modulePath = mainFile
    , exeScope = ExecutablePublic
    , buildInfo = testBuildInfo test
    }
testSuiteExeV10AsExe TestSuite{} = error "testSuiteExeV10AsExe: wrong kind"

-- | Translate a exe-style 'Benchmark' component into an exe for building
benchmarkExeV10asExe :: Benchmark -> Executable
benchmarkExeV10asExe bm@Benchmark{benchmarkInterface = BenchmarkExeV10 _ mainFile} =
  Executable
    { exeName = benchmarkName bm
    , modulePath = mainFile
    , exeScope = ExecutablePublic
    , buildInfo = benchmarkBuildInfo bm
    }
benchmarkExeV10asExe Benchmark{} = error "benchmarkExeV10asExe: wrong kind"

-- | Translate a lib-style 'TestSuite' component into a lib + exe for building
testSuiteLibV09AsLibAndExe
  :: PackageDescription
  -> TestSuite
  -> ComponentLocalBuildInfo
  -> LocalBuildInfo
  -> FilePath
  -- ^ absolute inplace dir
  -> SymbolicPath Pkg (Dir Dist)
  -> ( PackageDescription
     , Library
     , ComponentLocalBuildInfo
     , LocalBuildInfo
     , IPI.InstalledPackageInfo
     , Executable
     , ComponentLocalBuildInfo
     )
testSuiteLibV09AsLibAndExe
  pkg_descr
  test@TestSuite{testInterface = TestSuiteLibV09 _ m}
  clbi
  lbi
  inplaceDir
  distPref =
    (pkg, lib, libClbi, lbi, ipi, exe, exeClbi)
    where
      bi = testBuildInfo test
      lib =
        Library
          { libName = LMainLibName
          , exposedModules = [m]
          , reexportedModules = []
          , signatures = []
          , libExposed = True
          , libVisibility = LibraryVisibilityPrivate
          , libBuildInfo = bi
          }
      -- This is, like, the one place where we use a CTestName for a library.
      -- Should NOT use library name, since that could conflict!
      PackageIdentifier pkg_name pkg_ver = package pkg_descr
      -- Note: we do make internal library from the test!
      compat_name = MungedPackageName pkg_name (LSubLibName (testName test))
      compat_key = computeCompatPackageKey (compiler lbi) compat_name pkg_ver (componentUnitId clbi)
      libClbi =
        LibComponentLocalBuildInfo
          { componentPackageDeps = componentPackageDeps clbi
          , componentInternalDeps = componentInternalDeps clbi
          , componentIsIndefinite_ = False
          , componentExeDeps = componentExeDeps clbi
          , componentLocalName = CLibName $ LSubLibName $ testName test
          , componentIsPublic = False
          , componentIncludes = componentIncludes clbi
          , componentUnitId = componentUnitId clbi
          , componentComponentId = componentComponentId clbi
          , componentInstantiatedWith = []
          , componentCompatPackageName = compat_name
          , componentCompatPackageKey = compat_key
          , componentExposedModules = [IPI.ExposedModule m Nothing]
          }
      pkgName' = mkPackageName $ prettyShow compat_name
      pkg =
        pkg_descr
          { package = (package pkg_descr){pkgName = pkgName'}
          , executables = []
          , testSuites = []
          , subLibraries = [lib]
          }
      ipi = inplaceInstalledPackageInfo inplaceDir distPref pkg (mkAbiHash "") lib lbi libClbi
      testLibDep =
        Dependency
          pkgName'
          (thisVersion $ pkgVersion $ package pkg_descr)
          mainLibSet
      exe =
        Executable
          { exeName = mkUnqualComponentName $ stubName test
          , modulePath = makeRelativePathEx $ stubFilePath test
          , exeScope = ExecutablePublic
          , buildInfo =
              (testBuildInfo test)
                { hsSourceDirs = [coerceSymbolicPath $ testBuildDir lbi test]
                , targetBuildDepends =
                    testLibDep
                      : targetBuildDepends (testBuildInfo test)
                }
          }
      -- \| The stub executable needs a new 'ComponentLocalBuildInfo'
      -- that exposes the relevant test suite library.
      deps =
        (IPI.installedUnitId ipi, mungedId ipi)
          : ( filter
                ( \(_, x) ->
                    let name = prettyShow $ mungedName x
                     in name == "Cabal" || name == "base"
                )
                (componentPackageDeps clbi)
            )
      exeClbi =
        ExeComponentLocalBuildInfo
          { -- TODO: this is a hack, but as long as this is unique
            -- (doesn't clobber something) we won't run into trouble
            componentUnitId = mkUnitId (stubName test)
          , componentComponentId = mkComponentId (stubName test)
          , componentInternalDeps = [componentUnitId clbi]
          , componentExeDeps = []
          , componentLocalName = CExeName $ mkUnqualComponentName $ stubName test
          , componentPackageDeps = deps
          , -- Assert DefUnitId invariant!
            -- Executable can't be indefinite, so dependencies must
            -- be definite packages.
            componentIncludes =
              map ((,defaultRenaming) . DefiniteUnitId . unsafeMkDefUnitId . fst) deps
          }
testSuiteLibV09AsLibAndExe _ TestSuite{} _ _ _ _ = error "testSuiteLibV09AsLibAndExe: wrong kind"

-- | Initialize a new package db file for libraries defined
-- internally to the package.
createInternalPackageDB
  :: Verbosity
  -> LocalBuildInfo
  -> SymbolicPath Pkg (Dir Dist)
  -> IO PackageDB
createInternalPackageDB verbosity lbi distPref = do
  existsAlready <- doesPackageDBExist dbPath
  when existsAlready $ deletePackageDB dbPath
  createPackageDB verbosity (compiler lbi) (withPrograms lbi) False dbPath
  return (SpecificPackageDB dbPath)
  where
    dbRelPath = internalPackageDBPath lbi distPref
    dbPath = interpretSymbolicPathLBI lbi dbRelPath

-- | Update the program database to include any build-tool-depends specified
-- in the given 'BuildInfo' on build tools internal to the current package.
--
-- This function:
--
--  - adds these internal build tools to the 'ProgramDb', including
--    paths to their respective data directories,
--  - adds their paths to the current 'progSearchPath', and adds the data
--    directory environment variable for the current package to the current
--    'progOverrideEnv', so that any programs configured from now on will be
--    able to invoke these build tools.
addInternalBuildTools
  :: PackageDescription
  -> LocalBuildInfo
  -> BuildInfo
  -> ProgramDb
  -> ProgramDb
addInternalBuildTools pkg lbi bi progs =
  prependProgramSearchPathNoLogging
    internalToolPaths
    [pkgDataDirVar]
    $ foldr updateProgram progs internalBuildTools
  where
    internalToolPaths = map (takeDirectory . programPath) internalBuildTools
    pkgDataDirVar = (pkgPathEnvVar pkg "datadir", Just dataDirPath)
    internalBuildTools =
      [ (simpleConfiguredProgram toolName' (FoundOnSystem toolLocation))
        { programOverrideEnv = [pkgDataDirVar]
        }
      | toolName <- getAllInternalToolDependencies pkg bi
      , let toolName' = unUnqualComponentName toolName
      , let toolLocation =
              interpretSymbolicPathLBI lbi $
                buildDir lbi
                  </> makeRelativePathEx (toolName' </> toolName' <.> exeExtension (hostPlatform lbi))
      ]
    mbWorkDir = mbWorkDirLBI lbi
    rawDataDir = dataDir pkg
    dataDirPath
      | null $ getSymbolicPath rawDataDir =
          interpretSymbolicPath mbWorkDir sameDirectory
      | otherwise =
          interpretSymbolicPath mbWorkDir rawDataDir

-- TODO: build separate libs in separate dirs so that we can build
-- multiple libs, e.g. for 'LibTest' library-style test suites
buildLib
  :: BuildFlags
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
buildLib flags numJobs pkg_descr lbi lib clbi =
  let verbosity = fromFlag $ buildVerbosity flags
   in case compilerFlavor (compiler lbi) of
        GHC -> GHC.buildLib flags numJobs pkg_descr lbi lib clbi
        GHCJS -> GHCJS.buildLib verbosity numJobs pkg_descr lbi lib clbi
        UHC -> UHC.buildLib verbosity pkg_descr lbi lib clbi
        HaskellSuite{} -> HaskellSuite.buildLib verbosity pkg_descr lbi lib clbi
        _ -> dieWithException verbosity BuildingNotSupportedWithCompiler

-- | Build a foreign library
--
-- NOTE: We assume that we already checked that we can actually build the
-- foreign library in configure.
buildFLib
  :: Verbosity
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> ForeignLib
  -> ComponentLocalBuildInfo
  -> IO ()
buildFLib verbosity numJobs pkg_descr lbi flib clbi =
  case compilerFlavor (compiler lbi) of
    GHC -> GHC.buildFLib verbosity numJobs pkg_descr lbi flib clbi
    _ -> dieWithException verbosity BuildingNotSupportedWithCompiler

buildExe
  :: Verbosity
  -> Flag ParStrat
  -> PackageDescription
  -> LocalBuildInfo
  -> Executable
  -> ComponentLocalBuildInfo
  -> IO ()
buildExe verbosity numJobs pkg_descr lbi exe clbi =
  case compilerFlavor (compiler lbi) of
    GHC -> GHC.buildExe verbosity numJobs pkg_descr lbi exe clbi
    GHCJS -> GHCJS.buildExe verbosity numJobs pkg_descr lbi exe clbi
    UHC -> UHC.buildExe verbosity pkg_descr lbi exe clbi
    _ -> dieWithException verbosity BuildingNotSupportedWithCompiler

replLib
  :: ReplFlags
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
replLib replFlags pkg_descr lbi lib clbi =
  let verbosity = fromFlag $ replVerbosity replFlags
      opts = replReplOptions replFlags
   in case compilerFlavor (compiler lbi) of
        -- 'cabal repl' doesn't need to support 'ghc --make -j', so we just pass
        -- NoFlag as the numJobs parameter.
        GHC -> GHC.replLib replFlags NoFlag pkg_descr lbi lib clbi
        GHCJS -> GHCJS.replLib (replOptionsFlags opts) verbosity NoFlag pkg_descr lbi lib clbi
        _ -> dieWithException verbosity REPLNotSupported

replExe
  :: ReplFlags
  -> PackageDescription
  -> LocalBuildInfo
  -> Executable
  -> ComponentLocalBuildInfo
  -> IO ()
replExe flags pkg_descr lbi exe clbi =
  let verbosity = fromFlag $ replVerbosity flags
   in case compilerFlavor (compiler lbi) of
        GHC -> GHC.replExe flags NoFlag pkg_descr lbi exe clbi
        GHCJS ->
          GHCJS.replExe
            (replOptionsFlags $ replReplOptions flags)
            verbosity
            NoFlag
            pkg_descr
            lbi
            exe
            clbi
        _ -> dieWithException verbosity REPLNotSupported

replFLib
  :: ReplFlags
  -> PackageDescription
  -> LocalBuildInfo
  -> ForeignLib
  -> ComponentLocalBuildInfo
  -> IO ()
replFLib flags pkg_descr lbi exe clbi =
  let verbosity = fromFlag $ replVerbosity flags
   in case compilerFlavor (compiler lbi) of
        GHC -> GHC.replFLib flags NoFlag pkg_descr lbi exe clbi
        _ -> dieWithException verbosity REPLNotSupported

-- | Runs 'componentInitialBuildSteps' on every configured component.
--
-- Legacy function: does not run pre-build hooks or pre-processors. This function
-- is insufficient on its own to prepare the build for a package.
--
-- Consumers wanting to prepare the sources of a package, e.g. in order to
-- launch a REPL session, are advised to run @Setup repl --repl-multi-file=<fn>@
-- instead.
initialBuildSteps
  :: FilePath
  -- ^ "dist" prefix
  -> PackageDescription
  -- ^ mostly information from the .cabal file
  -> LocalBuildInfo
  -- ^ Configuration information
  -> Verbosity
  -- ^ The verbosity to use
  -> IO ()
initialBuildSteps distPref pkg_descr lbi verbosity =
  withAllComponentsInBuildOrder pkg_descr lbi $ \_comp clbi ->
    componentInitialBuildSteps distPref pkg_descr lbi clbi verbosity
{-# DEPRECATED
  initialBuildSteps
  "This function does not prepare all source files for a package. Suggestion: use 'Setup repl --repl-multi-file=<fn>'."
  #-}

-- | Creates the autogenerated files for a particular configured component.
--
-- Legacy function: does not run pre-build hooks or pre-processors. This function
-- is insufficient on its own to prepare the build for a component.
--
-- Consumers wanting to prepare the sources of a component, e.g. in order to
-- launch a REPL session, are advised to run
-- @Setup repl <compName> --repl-multi-file=<fn>@ instead.
componentInitialBuildSteps
  :: FilePath
  -- ^ "dist" prefix
  -> PackageDescription
  -- ^ mostly information from the .cabal file
  -> LocalBuildInfo
  -- ^ Configuration information
  -> ComponentLocalBuildInfo
  -- ^ Build info about the component
  -> Verbosity
  -- ^ The verbosity to use
  -> IO ()
componentInitialBuildSteps _distPref pkg_descr lbi clbi verbosity = do
  let compBuildDir = interpretSymbolicPathLBI lbi $ componentBuildDir lbi clbi
  createDirectoryIfMissingVerbose verbosity True compBuildDir
  writeBuiltinAutogenFiles verbosity pkg_descr lbi clbi
{-# DEPRECATED
  componentInitialBuildSteps
  "This function does not prepare all source files for a component. Suggestion: use 'Setup repl <compName> --repl-multi-file=<fn>'."
  #-}

-- | Creates the autogenerated files for a particular configured component,
-- and runs the pre-build hook.
preBuildComponent
  :: IO r
  -- ^ pre-build hook
  -> Verbosity
  -> LocalBuildInfo
  -- ^ Configuration information
  -> TargetInfo
  -> IO r
preBuildComponent preBuildHook verbosity lbi tgt = do
  let pkg_descr = localPkgDescr lbi
      clbi = targetCLBI tgt
      compBuildDir = interpretSymbolicPathLBI lbi $ componentBuildDir lbi clbi
  createDirectoryIfMissingVerbose verbosity True compBuildDir
  writeBuiltinAutogenFiles verbosity pkg_descr lbi clbi
  preBuildHook

-- | Generate and write to disk all built-in autogenerated files
-- for the specified component. These files will be put in the
-- autogenerated module directory for this component
-- (see 'autogenComponentsModuleDir').
--
-- This includes:
--
--  - @Paths_<pkg>.hs@,
--  - @PackageInfo_<pkg>.hs@,
--  - Backpack signature files for components that are not fully instantiated,
--  - @cabal_macros.h@.
writeBuiltinAutogenFiles
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> IO ()
writeBuiltinAutogenFiles verbosity pkg lbi clbi =
  writeAutogenFiles verbosity lbi clbi $ builtinAutogenFiles pkg lbi clbi

-- | Built-in autogenerated files and their contents. This includes:
--
--  - @Paths_<pkg>.hs@,
--  - @PackageInfo_<pkg>.hs@,
--  - Backpack signature files for components that are not fully instantiated,
--  - @cabal_macros.h@.
builtinAutogenFiles
  :: PackageDescription
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> Map AutogenFile AutogenFileContents
builtinAutogenFiles pkg lbi clbi =
  Map.insert pathsFile pathsContents $
    Map.insert packageInfoFile packageInfoContents $
      Map.insert cppHeaderFile cppHeaderContents $
        emptySignatureModules clbi
  where
    pathsFile = AutogenModule (autogenPathsModuleName pkg) (Suffix "hs")
    pathsContents = toUTF8LBS $ generatePathsModule pkg lbi clbi
    packageInfoFile = AutogenModule (autogenPackageInfoModuleName pkg) (Suffix "hs")
    packageInfoContents = toUTF8LBS $ generatePackageInfoModule pkg lbi
    cppHeaderFile = AutogenFile $ toShortText cppHeaderName
    cppHeaderContents = toUTF8LBS $ generateCabalMacrosHeader pkg lbi clbi

-- | An empty @".hsig"@ Backpack signature module for each requirement, so that
-- GHC has a source file to look at it when it needs to typecheck
-- a signature.  It's harmless to generate these modules, even when
-- there is a real @hsig@ file written by the user, since
-- include path ordering ensures that the real @hsig@ file
-- will always be picked up before the autogenerated one.
emptySignatureModules
  :: ComponentLocalBuildInfo
  -> Map AutogenFile AutogenFileContents
emptySignatureModules clbi =
  case clbi of
    LibComponentLocalBuildInfo{componentInstantiatedWith = insts} ->
      Map.fromList
        [ ( AutogenModule modName (Suffix "hsig")
          , emptyHsigFile modName
          )
        | (modName, _) <- insts
        ]
    _ -> Map.empty
  where
    emptyHsigFile :: ModuleName -> AutogenFileContents
    emptyHsigFile modName =
      toUTF8LBS $
        "{-# OPTIONS_GHC -w #-}\n"
          ++ "{-# LANGUAGE NoImplicitPrelude #-}\n"
          ++ "signature "
          ++ prettyShow modName
          ++ " where"

data AutogenFile
  = AutogenModule !ModuleName !Suffix
  | AutogenFile !ShortText
  deriving (Show, Eq, Ord)

-- | A representation of the contents of an autogenerated file.
type AutogenFileContents = LBS.ByteString

-- | Write the given autogenerated files in the autogenerated modules
-- directory for the component.
writeAutogenFiles
  :: Verbosity
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> Map AutogenFile AutogenFileContents
  -> IO ()
writeAutogenFiles verbosity lbi clbi autogenFiles = do
  -- Ensure that the overall autogenerated files directory exists.
  createDirectoryIfMissingVerbose verbosity True autogenDir
  for_ (Map.assocs autogenFiles) $ \(file, contents) -> do
    let path = case file of
          AutogenModule modName (Suffix ext) ->
            autogenDir </> ModuleName.toFilePath modName <.> ext
          AutogenFile fileName ->
            autogenDir </> fromShortText fileName
        dir = takeDirectory path
    -- Ensure that the directory subtree for this autogenerated file exists.
    createDirectoryIfMissingVerbose verbosity True dir
    -- Write the contents of the file.
    rewriteFileLBS verbosity path contents
  where
    autogenDir = interpretSymbolicPathLBI lbi $ autogenComponentModulesDir lbi clbi
