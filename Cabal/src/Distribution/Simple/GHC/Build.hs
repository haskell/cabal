{-# LANGUAGE DataKinds #-}

module Distribution.Simple.GHC.Build where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad.IO.Class
import Distribution.PackageDescription as PD hiding (buildInfo)
import Distribution.Simple.Build.Inputs
import Distribution.Simple.Flag (Flag)
import Distribution.Simple.GHC.Build.ExtraSources
import Distribution.Simple.GHC.Build.Link
import Distribution.Simple.GHC.Build.Modules
import Distribution.Simple.GHC.Build.Utils (compilerBuildWay, isHaskell, withDynFLib)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Builtin (ghcProgram)
import Distribution.Simple.Program.Db (requireProgram)
import Distribution.Simple.Utils

import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.PackageName.Magic (fakePackageId)
import Distribution.Types.ParStrat
import Distribution.Utils.NubList (fromNubListR)
import Distribution.Utils.Path

import Distribution.Verbosity (VerbosityHandles, mkVerbosity, verbosityHandles)
import System.FilePath (splitDirectories)

{- Note [Build Target Dir vs Target Dir]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Where to place the build result (targetDir) and the build artifacts (buildTargetDir).

\* For libraries, targetDir == buildTargetDir, where both the library and
artifacts are put together.

\* For executables or foreign libs, buildTargetDir == targetDir/<name-of-target-dir>-tmp, where
    the targetDir is the location where the target (e.g. the executable) is written to
    and buildTargetDir is where the compilation artifacts (e.g. Main.o) will live
  Arguably, this difference should not exist (#9498) (TODO)

For instance, for a component `cabal-benchmarks`:
  targetDir == <buildDir>/cabal-benchmarks
  buildTargetDir == <buildDir>/cabal-benchmarks/cabal-benchmarks-tmp

Or, for a library `Cabal`:
  targetDir == <buildDir>/.
  buildTargetDir == targetDir

Furthermore, we need to account for the limit of characters in ghc
invocations that different OSes constrain us to. Cabal invocations can
rapidly reach this limit, in part, due to the long length of cabal v2
prefixes. To minimize the likelihood, we use
`tryMakeRelativeToWorkingDir` to shorten the paths used in invocations
(see da6321bb).

However, in executables, we don't do this. It seems that we don't need to do it
for executable-like components because the linking step, instead of passing as
an argument the path to each module, it simply passes the module name, the sources dir, and --make.
RM: I believe we can use --make + module names instead of paths-to-objects
for linking libraries too (2024-01) (TODO)
-}

-- | The main build phase of building a component.
-- Includes building Haskell modules, extra build sources, and linking.
build
  :: Flag ParStrat
  -> VerbosityHandles
  -> PackageDescription
  -> PreBuildComponentInputs
  -- ^ The context and component being built in it.
  -> IO ()
build numJobs verbHandles pkg_descr pbci = do
  let
    verbosity = mkVerbosity verbHandles $ buildVerbosity pbci
    isLib = buildIsLib pbci
    lbi = localBuildInfo pbci
    bi = buildBI pbci
    clbi = buildCLBI pbci
    isIndef = componentIsIndefinite clbi
    mbWorkDir = mbWorkDirLBI lbi
    i = interpretSymbolicPathLBI lbi -- See Note [Symbolic paths] in Distribution.Utils.Path

  -- Create a few directories for building the component
  -- See Note [Build Target Dir vs Target Dir]
  let targetDir0 :: SymbolicPath Pkg ('Dir Build)
      targetDir0 = componentBuildDir lbi clbi
      buildTargetDir0 :: SymbolicPath Pkg ('Dir Artifacts)
      buildTargetDir0
        -- Libraries use the target dir for building (see above)
        | isLib = coerceSymbolicPath targetDir0
        -- In other cases, use targetDir/<name-of-target-dir>-tmp
        | targetDirName : _ <- reverse $ splitDirectories $ getSymbolicPath targetDir0 =
            coerceSymbolicPath targetDir0 </> makeRelativePathEx (targetDirName ++ "-tmp")
        | otherwise = error "GHC.build: targetDir is empty"

  liftIO $ do
    createDirectoryIfMissingVerbose verbosity True $ i targetDir0
    createDirectoryIfMissingVerbose verbosity True $ i buildTargetDir0

  -- See Note [Build Target Dir vs Target Dir] as well
  let targetDir = targetDir0 -- NB: no 'makeRelative'
  buildTargetDir <-
    if isLib
      then -- NB: this might fail to make the buildTargetDir relative,
      -- as noted in #9776. Oh well.
        tryMakeRelative mbWorkDir buildTargetDir0
      else return buildTargetDir0
  -- To preserve the previous behaviour, we don't use relative dirs for
  -- executables. Historically, this isn't needed to reduce the CLI limit
  -- (unlike for libraries) because we link executables with the module names
  -- instead of passing the path to object file -- that's something else we
  -- can now fix after the refactor lands.

  (ghcProg, _) <- liftIO $ requireProgram verbosity ghcProgram (withPrograms lbi)

  -- Ways which are wanted from configuration flags
  let wantedWays@(wantedLibWays, wantedFLibWay, wantedExeWay) = buildWays lbi

  -- Ways which are needed due to the compiler configuration
  let doingTH = usesTemplateHaskellOrQQ bi
      defaultGhcWay = compilerBuildWay (buildCompiler pbci)
      wantedModBuildWays = case buildComponent pbci of
        CLib _ -> wantedLibWays isIndef
        CFLib fl -> [wantedFLibWay (withDynFLib fl)]
        CExe _ -> [wantedExeWay]
        CTest _ -> [wantedExeWay]
        CBench _ -> [wantedExeWay]
      finalModBuildWays =
        wantedModBuildWays
          ++ [defaultGhcWay | doingTH && defaultGhcWay `notElem` wantedModBuildWays]
      compNameStr = showComponentName $ componentName $ buildComponent pbci

  liftIO $ info verbosity ("Wanted module build ways(" ++ compNameStr ++ "): " ++ show wantedModBuildWays)
  liftIO $ info verbosity ("Final module build ways(" ++ compNameStr ++ "): " ++ show finalModBuildWays)
  -- We need a separate build and link phase, and C sources must be compiled
  -- after Haskell modules, because C sources may depend on stub headers
  -- generated from compiling Haskell modules (#842, #3294).
  (mbMainFile, inputModules) <- componentInputs buildTargetDir verbHandles pkg_descr pbci
  let (hsMainFile, nonHsMainFile) =
        case mbMainFile of
          Just mainFile
            | PD.package pkg_descr == fakePackageId
                || isHaskell (getSymbolicPath mainFile) ->
                (Just mainFile, Nothing)
            | otherwise ->
                (Nothing, Just mainFile)
          Nothing -> (Nothing, Nothing)
  buildOpts <- buildHaskellModules numJobs ghcProg hsMainFile inputModules buildTargetDir finalModBuildWays verbHandles pbci
  extraSources <- buildAllExtraSources nonHsMainFile ghcProg buildTargetDir wantedWays verbHandles pbci
  linkOrLoadComponent
    ghcProg
    (verbosityHandles verbosity)
    pkg_descr
    (fromNubListR extraSources)
    (buildTargetDir, targetDir)
    (wantedWays, buildOpts)
    pbci
