{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Distribution.Simple.GHC.Build.Modules (buildHaskellModules, BuildWay (..), buildWayPrefix) where

import Control.Monad.IO.Class
import Distribution.Compat.Prelude

import Data.List (sortOn, (\\))
import qualified Data.Set as Set
import Distribution.CabalSpecVersion
import Distribution.ModuleName (ModuleName)
import qualified Distribution.PackageDescription as PD
import Distribution.Pretty
import Distribution.Simple.Build.Inputs
import Distribution.Simple.Compiler
import Distribution.Simple.GHC.Build.Utils
import qualified Distribution.Simple.GHC.Internal as Internal
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.GHC
import Distribution.Simple.Program.Types
import Distribution.Simple.Setup.Common
import Distribution.Simple.Utils
import Distribution.Types.Benchmark
import Distribution.Types.BenchmarkInterface
import Distribution.Types.BuildInfo
import Distribution.Types.Executable
import Distribution.Types.ForeignLib
import Distribution.Types.PackageName.Magic
import Distribution.Types.ParStrat
import Distribution.Types.TestSuite
import Distribution.Types.TestSuiteInterface
import Distribution.Utils.NubList
import Distribution.Utils.Path
import System.FilePath ()

{-
Note [Building Haskell Modules accounting for TH]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are multiple ways in which we may want to build our Haskell modules:
  * The static way (-static)
  * The dynamic/shared way (-dynamic)
  * The profiled way (-prof)

For libraries, we may /want/ to build modules in all three ways, or in any combination, depending on user options.
For executables, we just /want/ to build the executable in the requested way.

In practice, however, we may /need/ to build modules in additional ways beyonds the ones that were requested.
This can happen because of Template Haskell.

When we're using Template Haskell, we /need/ to additionally build modules with
the used GHC's default/vanilla ABI. This is because the code that TH needs to
run at compile time needs to be the vanilla ABI so it can be loaded up and run
by the compiler. With dynamic-by-default GHC the TH object files loaded at
compile-time need to be .dyn_o instead of .o.

  * If the GHC is dynamic by default, that means we may need to also build
  the dynamic way in addition the wanted way.

  * If the GHC is static by default, we may need to build statically additionally.

Of course, if the /wanted/ way is the way additionally /needed/ for TH, we don't need to do extra work.

If it turns out that in the end we need to build both statically and
dynamically, we want to make use of GHC's -static -dynamic-too capability, which
builds modules in the two ways in a single invocation.

If --dynamic-too is not supported by the GHC, then we need to be careful about
the order in which modules are built. Specifically, we must first build the
modules for TH with the vanilla ABI, and only afterwards the desired
(non-default) ways.

A few examples:

To build an executable with profiling, with a dynamic by default GHC, and TH is used:
  * Build dynamic (needed) objects
  * Build profiled objects

To build a library with profiling and dynamically, with a static by default GHC, and TH is used:
  * Build dynamic (wanted) and static (needed) objects together with --dynamic-too
  * Build profiled objects

To build an executable statically, with a static by default GHC, regardless of whether TH is used:
  * Simply build static objects

-}

-- | Compile the Haskell modules of the component being built.
buildHaskellModules
  :: Flag ParStrat
  -- ^ The parallelism strategy (e.g. num of jobs)
  -> ConfiguredProgram
  -- ^ The GHC configured program
  -> PD.PackageDescription
  -- ^ The package description
  -> SymbolicPath Pkg ('Dir Artifacts)
  -- ^ The path to the build directory for this target, which
  -- has already been created.
  -> Set.Set BuildWay
  -- ^ The set of wanted build ways according to user options
  -> PreBuildComponentInputs
  -- ^ The context and component being built in it.
  -> IO (BuildWay -> GhcOptions)
  -- ^ Returns a mapping from build ways to the 'GhcOptions' used in the
  -- invocation used to compile the component in that 'BuildWay'.
  -- This can be useful in, eg, a linker invocation, in which we want to use the
  -- same options and list the same inputs as those used for building.
buildHaskellModules numJobs ghcProg pkg_descr buildTargetDir wantedWays pbci = do
  -- See Note [Building Haskell Modules accounting for TH]

  let
    verbosity = buildVerbosity pbci
    isLib = buildIsLib pbci
    clbi = buildCLBI pbci
    lbi = localBuildInfo pbci
    bi = buildBI pbci
    what = buildingWhat pbci
    comp = buildCompiler pbci
    i = interpretSymbolicPathLBI lbi -- See Note [Symbolic paths] in Distribution.Utils.Path

    -- If this component will be loaded into a repl, we don't compile the modules at all.
    forRepl
      | BuildRepl{} <- what = True
      | otherwise = False

  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?  FIX: what about exeName.hi-boot?

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = if isLib then libCoverage lbi else exeCoverage lbi
      hpcdir way
        | forRepl = mempty -- HPC is not supported in ghci
        | isCoverageEnabled = Flag $ Hpc.mixDir (coerceSymbolicPath $ coerceSymbolicPath buildTargetDir </> extraCompilationArtifacts) way
        | otherwise = mempty

  (inputFiles, inputModules) <- componentInputs buildTargetDir pkg_descr pbci

  let
    mbWorkDir = mbWorkDirLBI lbi
    runGhcProg = runGHC verbosity ghcProg comp platform mbWorkDir
    platform = hostPlatform lbi

    -- See Note [Building Haskell Modules accounting for TH]
    doingTH = usesTemplateHaskellOrQQ bi

    -- We define the base opts which are shared across different build ways in
    -- 'buildHaskellModules'
    baseOpts way =
      (Internal.componentGhcOptions verbosity lbi bi clbi buildTargetDir)
        `mappend` mempty
          { ghcOptMode = toFlag GhcModeMake
          , -- Previously we didn't pass -no-link when building libs,
            -- but I think that could result in a bug (e.g. if a lib module is
            -- called Main and exports main). So we really want nolink when
            -- building libs too (TODO).
            ghcOptNoLink = if isLib then NoFlag else toFlag True
          , ghcOptNumJobs = numJobs
          , ghcOptInputModules = toNubListR inputModules
          , ghcOptInputFiles =
              toNubListR $
                if PD.package pkg_descr == fakePackageId
                  then filter (isHaskell . getSymbolicPath) inputFiles
                  else inputFiles
          , ghcOptInputScripts =
              toNubListR $
                if PD.package pkg_descr == fakePackageId
                  then filter (not . isHaskell . getSymbolicPath) inputFiles
                  else []
          , ghcOptExtra = buildWayExtraHcOptions way GHC bi
          , ghcOptHiSuffix = optSuffixFlag (buildWayPrefix way) "hi"
          , ghcOptObjSuffix = optSuffixFlag (buildWayPrefix way) "o"
          , ghcOptHPCDir = hpcdir (buildWayHpcWay way) -- maybe this should not be passed for vanilla?
          }
      where
        optSuffixFlag "" _ = NoFlag
        optSuffixFlag pre x = toFlag (pre ++ x)

    -- For libs we don't pass -static when building static, leaving it
    -- implicit. We should just always pass -static, but we don't want to
    -- change behaviour when doing the refactor.
    staticOpts = (baseOpts StaticWay){ghcOptDynLinkMode = if isLib then NoFlag else toFlag GhcStaticOnly}
    dynOpts =
      (baseOpts DynWay)
        { ghcOptDynLinkMode = toFlag GhcDynamicOnly -- use -dynamic
        , -- TODO: Does it hurt to set -fPIC for executables?
          ghcOptFPic = toFlag True -- use -fPIC
        }
    profOpts =
      (baseOpts ProfWay)
        { ghcOptProfilingMode = toFlag True
        , ghcOptProfilingAuto =
            Internal.profDetailLevelFlag
              (if isLib then True else False)
              ((if isLib then withProfLibDetail else withProfExeDetail) lbi)
        }
    -- Options for building both static and dynamic way at the same time, using
    -- the GHC flag -static and -dynamic-too
    dynTooOpts =
      (baseOpts StaticWay)
        { ghcOptDynLinkMode = toFlag GhcStaticAndDynamic -- use -dynamic-too
        , ghcOptDynHiSuffix = toFlag (buildWayPrefix DynWay ++ "hi")
        , ghcOptDynObjSuffix = toFlag (buildWayPrefix DynWay ++ "o")
        , ghcOptHPCDir = hpcdir Hpc.Dyn
        -- Should we pass hcSharedOpts in the -dynamic-too ghc invocation?
        -- (Note that `baseOtps StaticWay = hcStaticOptions`, not hcSharedOpts)
        }

    -- Determines how to build for each way, also serves as the base options
    -- for loading modules in 'linkOrLoadComponent'
    buildOpts way = case way of
      StaticWay -> staticOpts
      DynWay -> dynOpts
      ProfWay -> profOpts

    defaultGhcWay = if isDynamic comp then DynWay else StaticWay

  -- If there aren't modules, or if we're loading the modules in repl, don't build.
  unless (forRepl || (null inputFiles && null inputModules)) $ liftIO $ do
    -- See Note [Building Haskell Modules accounting for TH]
    let
      neededWays =
        wantedWays
          <> Set.fromList
            -- TODO: You also don't need to build the GHC way when doing TH if
            -- you are using an external interpreter!!
            [defaultGhcWay | doingTH && defaultGhcWay `Set.notMember` wantedWays]

      -- If we need both static and dynamic, use dynamic-too instead of
      -- compiling twice (if we support it)
      useDynamicToo =
        StaticWay `Set.member` neededWays
          && DynWay `Set.member` neededWays
          && supportsDynamicToo comp
          && null (hcSharedOptions GHC bi)

      -- The ways we'll build, in order
      orderedBuilds
        -- If we can use dynamic-too, do it first. The default GHC way can only
        -- be static or dynamic, so, if we build both right away, any modules
        -- possibly needed by TH later (e.g. if building profiled) are already built.
        | useDynamicToo =
            [buildStaticAndDynamicToo]
              ++ (runGhcProg . buildOpts <$> Set.toList neededWays \\ [StaticWay, DynWay])
        -- Otherwise, we need to ensure the defaultGhcWay is built first
        | otherwise =
            runGhcProg . buildOpts <$> sortOn (\w -> if w == defaultGhcWay then 0 else fromEnum w + 1) (Set.toList neededWays)

      buildStaticAndDynamicToo = do
        runGhcProg dynTooOpts
        case (hpcdir Hpc.Dyn, hpcdir Hpc.Vanilla) of
          (Flag dynDir, Flag vanillaDir) ->
            -- When the vanilla and shared library builds are done
            -- in one pass, only one set of HPC module interfaces
            -- are generated. This set should suffice for both
            -- static and dynamically linked executables. We copy
            -- the modules interfaces so they are available under
            -- both ways.
            copyDirectoryRecursive verbosity (i dynDir) (i vanillaDir)
          _ -> return ()
     in
      -- REVIEW:ADD? info verbosity "Building Haskell Sources..."
      sequence_ orderedBuilds
  return buildOpts

data BuildWay = StaticWay | DynWay | ProfWay
  deriving (Eq, Ord, Show, Enum)

-- | Returns the object/interface extension prefix for the given build way (e.g. "dyn_" for 'DynWay')
buildWayPrefix :: BuildWay -> String
buildWayPrefix = \case
  StaticWay -> ""
  ProfWay -> "p_"
  DynWay -> "dyn_"

-- | Returns the corresponding 'Hpc.Way' for a 'BuildWay'
buildWayHpcWay :: BuildWay -> Hpc.Way
buildWayHpcWay = \case
  StaticWay -> Hpc.Vanilla
  ProfWay -> Hpc.Prof
  DynWay -> Hpc.Dyn

-- | Returns a function to extract the extra haskell compiler options from a
-- 'BuildInfo' and 'CompilerFlavor'
buildWayExtraHcOptions :: BuildWay -> CompilerFlavor -> BuildInfo -> [String]
buildWayExtraHcOptions = \case
  StaticWay -> hcStaticOptions
  ProfWay -> hcProfOptions
  DynWay -> hcSharedOptions

-- | Returns a pair of the Haskell input files and Haskell modules of the
-- component being built.
--
-- The "input files" are either the path to the main Haskell module, or a repl
-- script (that does not necessarily have an extension).
componentInputs
  :: SymbolicPath Pkg (Dir Artifacts)
  -- ^ Target build dir
  -> PD.PackageDescription
  -> PreBuildComponentInputs
  -- ^ The context and component being built in it.
  -> IO ([SymbolicPath Pkg File], [ModuleName])
  -- ^ The Haskell input files, and the Haskell modules
componentInputs buildTargetDir pkg_descr pbci =
  case component of
    CLib lib ->
      pure ([], allLibModules lib clbi)
    CFLib flib ->
      pure ([], foreignLibModules flib)
    CExe Executable{buildInfo = bi', modulePath} ->
      exeLikeInputs bi' modulePath
    CTest TestSuite{testBuildInfo = bi', testInterface = TestSuiteExeV10 _ mainFile} ->
      exeLikeInputs bi' mainFile
    CBench Benchmark{benchmarkBuildInfo = bi', benchmarkInterface = BenchmarkExeV10 _ mainFile} ->
      exeLikeInputs bi' mainFile
    CTest TestSuite{} -> error "testSuiteExeV10AsExe: wrong kind"
    CBench Benchmark{} -> error "benchmarkExeV10asExe: wrong kind"
  where
    verbosity = buildVerbosity pbci
    component = buildComponent pbci
    clbi = buildCLBI pbci
    mbWorkDir = mbWorkDirLBI $ localBuildInfo pbci
    exeLikeInputs bnfo modulePath = liftIO $ do
      main <- findExecutableMain verbosity mbWorkDir buildTargetDir (bnfo, modulePath)
      let mainModName = exeMainModuleName bnfo
          otherModNames = otherModules bnfo

      -- Scripts have fakePackageId and are always Haskell but can have any extension.
      if isHaskell (getSymbolicPath main) || PD.package pkg_descr == fakePackageId
        then
          if PD.specVersion pkg_descr < CabalSpecV2_0 && (mainModName `elem` otherModNames)
            then do
              -- The cabal manual clearly states that `other-modules` is
              -- intended for non-main modules.  However, there's at least one
              -- important package on Hackage (happy-1.19.5) which
              -- violates this. We workaround this here so that we don't
              -- invoke GHC with e.g.  'ghc --make Main src/Main.hs' which
              -- would result in GHC complaining about duplicate Main
              -- modules.
              --
              -- Finally, we only enable this workaround for
              -- specVersion < 2, as 'cabal-version:>=2.0' cabal files
              -- have no excuse anymore to keep doing it wrong... ;-)
              warn verbosity $
                "Enabling workaround for Main module '"
                  ++ prettyShow mainModName
                  ++ "' listed in 'other-modules' illegally!"
              return ([main], filter (/= mainModName) otherModNames)
            else return ([main], otherModNames)
        else return ([], otherModNames)
