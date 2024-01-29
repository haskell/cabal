module Distribution.Simple.GHC.Build where

import Distribution.Compat.Prelude
import Prelude ()

import Control.Monad.IO.Class
import qualified Data.Set as Set
import Distribution.PackageDescription as PD hiding (buildInfo)
import Distribution.Simple.Build.Inputs
import Distribution.Simple.Flag (Flag)
import Distribution.Simple.GHC.Build.ExtraSources
import Distribution.Simple.GHC.Build.Link
import Distribution.Simple.GHC.Build.Modules
import Distribution.Simple.GHC.Build.Utils (withDynFLib)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Types.ComponentLocalBuildInfo (componentIsIndefinite)
import Distribution.Types.ParStrat
import Distribution.Utils.NubList (fromNubListR)
import System.Directory hiding (exeExtension)
import System.FilePath

{-
Note [Build Target Dir vs Target Dir]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
`makeRelativeToCurrentDirectory` to shorten the paths used in invocations
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
  -> PackageDescription
  -> PreBuildComponentInputs
  -- ^ The context and component being built in it.
  -> IO ()
build numJobs pkg_descr pbci = do
  let
    verbosity = buildVerbosity pbci
    component = buildComponent pbci
    isLib = buildIsLib pbci
    lbi = localBuildInfo pbci
    clbi = buildCLBI pbci

  -- Create a few directories for building the component
  -- See Note [Build Target Dir vs Target Dir]
  let targetDir_absolute = componentBuildDir lbi clbi
      buildTargetDir_absolute
        -- Libraries use the target dir for building (see above)
        | isLib = targetDir_absolute
        -- In other cases, use targetDir/<name-of-target-dir>-tmp
        | targetDirName : _ <- reverse $ splitDirectories targetDir_absolute =
            targetDir_absolute </> (targetDirName ++ "-tmp")
        | otherwise = error "GHC.build: targetDir is empty"

  liftIO $ do
    createDirectoryIfMissingVerbose verbosity True targetDir_absolute
    createDirectoryIfMissingVerbose verbosity True buildTargetDir_absolute

  -- See Note [Build Target Dir vs Target Dir] as well
  _targetDir <- liftIO $ makeRelativeToCurrentDirectory targetDir_absolute
  buildTargetDir <-
    -- To preserve the previous behaviour, we don't use relative dirs for
    -- executables. Historically, this isn't needed to reduce the CLI limit
    -- (unlike for libraries) because we link executables with the module names
    -- instead of passing the path to object file -- that's something else we
    -- can now fix after the refactor lands.
    if isLib
      then liftIO $ makeRelativeToCurrentDirectory buildTargetDir_absolute
      else return buildTargetDir_absolute

  (ghcProg, _) <- liftIO $ requireProgram verbosity ghcProgram (withPrograms lbi)

  -- Determine in which ways we want to build the component
  let
    wantVanilla = if isLib then withVanillaLib lbi else False
    -- Arguably, wantStatic should be "withFullyStaticExe lbi" for executables,
    -- but it was not before the refactor.
    wantStatic = if isLib then withStaticLib lbi else not (wantDynamic || wantProf)
    wantDynamic = case component of
      CLib{} -> withSharedLib lbi
      CFLib flib -> withDynFLib flib
      CExe{} -> withDynExe lbi
      CTest{} -> withDynExe lbi
      CBench{} -> withDynExe lbi
    wantProf = if isLib then withProfLib lbi else withProfExe lbi

    -- See also Note [Building Haskell Modules accounting for TH] in Distribution.Simple.GHC.Build.Modules
    -- We build static by default if no other way is wanted.
    -- For executables and foreign libraries, there should only be one wanted way.
    wantedWays =
      Set.fromList $
        -- If building a library, we accumulate all the ways,
        -- otherwise, we take just one.
        (if isLib then id else take 1) $
          [ProfWay | wantProf]
            -- I don't see why we shouldn't build with dynamic
            -- indefinite components.
            <> [DynWay | wantDynamic && not (componentIsIndefinite clbi)]
            <> [StaticWay | wantStatic || wantVanilla || not (wantDynamic || wantProf)]

  liftIO $ info verbosity ("Wanted build ways: " ++ show (Set.toList wantedWays))

  -- We need a separate build and link phase, and C sources must be compiled
  -- after Haskell modules, because C sources may depend on stub headers
  -- generated from compiling Haskell modules (#842, #3294).
  buildOpts <- buildHaskellModules numJobs ghcProg pkg_descr buildTargetDir_absolute wantedWays pbci
  extraSources <- buildAllExtraSources ghcProg buildTargetDir pbci
  linkOrLoadComponent ghcProg pkg_descr (fromNubListR extraSources) (buildTargetDir, targetDir_absolute) (wantedWays, buildOpts) pbci
