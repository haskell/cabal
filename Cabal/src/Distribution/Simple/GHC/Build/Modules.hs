{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Simple.GHC.Build.Modules
  ( buildHaskellModules )
  where 

import Distribution.Compat.Prelude
import Control.Monad.IO.Class

import Distribution.Types.ParStrat
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Utils.NubList
import System.FilePath
import Distribution.Simple.Build.Monad
import Distribution.Simple.GHC.Build.Utils
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.Setup.Common
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.GHC
import Distribution.Types.ForeignLib
import Distribution.Types.Executable
import Distribution.Types.TestSuite
import Distribution.Types.Benchmark
import Distribution.Types.BuildInfo
import qualified Data.Set as Set
import Data.List ((\\), sortOn)
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.ModuleName (ModuleName)
import Distribution.Types.TestSuiteInterface
import Distribution.Types.BenchmarkInterface
import Distribution.Pretty
import Distribution.CabalSpecVersion
import Distribution.Types.PackageName.Magic
import qualified Distribution.PackageDescription as PD

{-
Note [Building Haskell Modules accounting for TH]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are multiple ways in which we may want to build our Haskell modules:
  * The static way
  * The dynamic/shared way
  * The profiled way

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
dynamically, we want to make use of GHC's --dynamic-too capability, which
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
buildHaskellModules :: Flag ParStrat
                    -- ^ The parallelism strategy (e.g. num of jobs)
                    -> ConfiguredProgram
                    -- ^ The GHC configured program
                    -> PD.PackageDescription
                    -- ^ The package description
                    -> FilePath
                    -- ^ The path to the build directory for this target, which
                    -- has already been created.
                    -> BuildM ()
-- See Note [Building Haskell Modules accounting for TH]
buildHaskellModules numJobs ghcProg pkg_descr buildTargetDir = do
  verbosity <- buildVerbosity
  component <- buildComponent
  clbi      <- buildCLBI
  lbi       <- buildLBI
  bi        <- buildBI
  what      <- buildWhat
  comp      <- buildCompiler

  let isLib | CLib{} <- component = True
            | otherwise = False
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
        | isCoverageEnabled = Flag $ Hpc.mixDir (buildTargetDir </> extraCompilationArtifacts) way
        | otherwise = mempty

  (inputFiles, inputModules) <- componentInputs buildTargetDir pkg_descr

  let
    runGhcProg = runGHC verbosity ghcProg comp platform
    platform = hostPlatform lbi

    -- See Note [Building Haskell Modules accounting for TH]
    doingTH = usesTemplateHaskellOrQQ bi

    baseOpts = Internal.componentGhcOptions verbosity lbi bi clbi buildTargetDir
    vanillaOpts = 
      baseOpts
        `mappend` mempty
          { ghcOptMode = toFlag GhcModeMake
          , ghcOptNumJobs = numJobs
          , ghcOptInputModules = toNubListR inputModules
          , ghcOptInputFiles =
              toNubListR $
                if PD.package pkg_descr == fakePackageId
                  then filter isHaskell inputFiles
                  else inputFiles
          , ghcOptInputScripts =
              toNubListR $
                if PD.package pkg_descr == fakePackageId
                  then filter (not . isHaskell) inputFiles
                  else []
          }

    staticOpts =
      vanillaOpts
        `mappend` mempty
          { ghcOptDynLinkMode = toFlag GhcStaticOnly
          , ghcOptHPCDir = hpcdir Hpc.Vanilla
          }
    dynOpts =
      vanillaOpts
        `mappend` mempty
          { ghcOptDynLinkMode = toFlag GhcDynamicOnly
            -- TODO: Does it hurt to set -fPIC for executables?
          , ghcOptFPic = toFlag True
          , ghcOptHiSuffix = toFlag "dyn_hi"
          , ghcOptObjSuffix = toFlag "dyn_o"
          , ghcOptExtra = hcSharedOptions GHC bi
          , ghcOptHPCDir = hpcdir Hpc.Dyn
          }
    profOpts =
      vanillaOpts
        `mappend` mempty
          { ghcOptProfilingMode = toFlag True
          , ghcOptProfilingAuto =
              Internal.profDetailLevelFlag
                (if isLib then True else False)
                ((if isLib then withProfLibDetail else withProfExeDetail) lbi)
          , ghcOptHiSuffix = toFlag "p_hi"
          , ghcOptObjSuffix = toFlag "p_o"
          , ghcOptExtra = hcProfOptions GHC bi
          , ghcOptHPCDir = hpcdir Hpc.Prof
          }
    dynTooOpts =
      vanillaOpts
        `mappend` mempty
          { ghcOptDynLinkMode = toFlag GhcStaticAndDynamic
          , ghcOptDynHiSuffix = toFlag "dyn_hi"
          , ghcOptDynObjSuffix = toFlag "dyn_o"
          , ghcOptHPCDir = hpcdir Hpc.Dyn
          }

    -- wantVanilla is underspecified, maybe we could deprecated it (TODO)
    wantVanilla = if isLib then withVanillaLib lbi else False
    wantStatic  = if isLib then withStaticLib lbi else withFullyStaticExe lbi
    wantDynamic = if isLib then withSharedLib lbi else withDynExe lbi
    wantProf    = if isLib then withProfLib lbi else withProfExe lbi

    defaultGhcWay = if isDynamic comp then DynWay else StaticWay

  -- If there aren't modules, or if we're loading the modules in repl, don't build.
  unless (forRepl || (null inputFiles && null inputModules)) $ liftIO $

    -- See Note [Building Haskell Modules accounting for TH]
    let
      wantedWays
        = Set.fromList
        $ [StaticWay | wantStatic]
        <> [DynWay | wantDynamic ]
        <> [ProfWay | wantProf ]
        -- If no way is explicitly wanted, we take vanilla
        <> [VanillaWay | wantVanilla || not (wantStatic || wantDynamic || wantProf) ]
        -- ROMES:TODO: Is vanilla necessarily the same as defaultGhcWay? If so,
        -- we can deal away with VanillaWay and be explicit in -dynamic vs
        -- -static, or always default to -static. Would simplify further.
        -- ROMES:TODO: Perhaps, if the component is indefinite, we only pick Vanilla?
        -- To mimick the old behaviour we need at least profiled too (Vanilla +
        -- Prof), and there's even a test for profiled signature, whatever that
        -- means. So only doing vanilla way for indefinite components before seems wrong.
        -- Consider...

      neededWays
        = wantedWays
        <> Set.fromList
            -- TODO: You also don't need this if you are using an external interpreter!!
            [defaultGhcWay | doingTH && defaultGhcWay `Set.notMember` wantedWays]

      -- If we need both static and dynamic, use dynamic-too instead of
      -- compiling twice (if we support it)
      useDynamicToo
        -- TODO: These vanilla way are kind of bothersome. Ask Matthew.
        = (StaticWay `Set.member` neededWays || VanillaWay `Set.member` neededWays)
        && DynWay `Set.member` neededWays
        && supportsDynamicToo comp
        && null (hcSharedOptions GHC bi)

      -- The ways we'll build, in order
      orderedBuilds

        -- If we can use dynamic-too, do it first. The default GHC way can only
        -- be static or dynamic, so if we build both right away any TH-needed
        -- modules possibly needed later (for prof.) are already built.
        | useDynamicToo
        = [ buildStaticAndDynamicToo ] ++
          (buildWay <$> Set.toList neededWays \\ [StaticWay, VanillaWay, DynWay])

        -- Otherwise, we need to ensure the defaultGhcWay is built first.
        | otherwise
        = buildWay <$> sortOn (\w -> if w == defaultGhcWay then 0 else 1 :: Int) (Set.toList neededWays)

      buildWay = \case
        StaticWay -> runGhcProg staticOpts
        DynWay    -> runGhcProg dynOpts
        ProfWay   -> runGhcProg profOpts
        VanillaWay -> runGhcProg vanillaOpts

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
            copyDirectoryRecursive verbosity dynDir vanillaDir
          _ -> return ()

    in sequence_ orderedBuilds

data BuildWay = StaticWay | DynWay | ProfWay | VanillaWay
  deriving (Eq, Ord)

-- | Returns a pair of the input files and Haskell modules of the component
-- being built.
componentInputs :: FilePath
                -- ^ Target build dir
                -> PD.PackageDescription
                -> BuildM ([FilePath], [ModuleName])
componentInputs buildTargetDir pkg_descr = do
  verbosity <- buildVerbosity
  component <- buildComponent
  clbi <- buildCLBI

  case component of
    CLib lib
      -> pure ([], allLibModules lib clbi)
    CFLib flib
      -> pure ([], foreignLibModules flib)
    CExe Executable{buildInfo=bi', modulePath}
      -> exeLikeInputs verbosity bi' modulePath
    CTest TestSuite{testBuildInfo=bi', testInterface = TestSuiteExeV10 _ mainFile }
      -> exeLikeInputs verbosity bi' mainFile
    CBench Benchmark{benchmarkBuildInfo=bi', benchmarkInterface = BenchmarkExeV10 _ mainFile }
      -> exeLikeInputs verbosity bi' mainFile
    CTest TestSuite{} -> error "testSuiteExeV10AsExe: wrong kind"
    CBench Benchmark{} -> error "benchmarkExeV10asExe: wrong kind"

  where
    exeLikeInputs verbosity bnfo modulePath = liftIO $ do
      main <- findExecutableMain verbosity buildTargetDir (bnfo, modulePath)
      let mainModName = exeMainModuleName bnfo
          otherModNames = otherModules bnfo

      -- Scripts have fakePackageId and are always Haskell but can have any extension.
      if isHaskell main || PD.package pkg_descr == fakePackageId then
        if PD.specVersion pkg_descr < CabalSpecV2_0 && (mainModName `elem` otherModNames) then do
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
        else
          return ([main], otherModNames)
      else
        return ([], otherModNames)
