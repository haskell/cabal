{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Distribution.Client.InLibrary
  ( libraryConfigureInputsFromElabPackage
  , configure
  , build
  , haddock
  , copy
  , register
  , repl
  , test
  , bench
  )
where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Backpack.DescribeUnitId (setupMessage')
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.RebuildMonad
import qualified Distribution.Client.SetupHooks.CallHooksExe as ExternalHooksExe
  ( buildTypePreBuildHooks
  , buildTypeSetupHooks
  )
import Distribution.Client.Types

import qualified Distribution.PackageDescription as PD
import Distribution.Simple (Compiler, PackageDBStackCWD)
import qualified Distribution.Simple.Bench as Cabal
import Distribution.Simple.Build (build_setupHooks, repl_setupHooks)
import qualified Distribution.Simple.Configure as Cabal
import Distribution.Simple.Haddock (haddock_setupHooks)
import Distribution.Simple.Install (install_setupHooks)
import Distribution.Simple.LocalBuildInfo (mbWorkDirLBI)
import qualified Distribution.Simple.PreProcess as Cabal
import Distribution.Simple.Program.Db
import qualified Distribution.Simple.Register as Cabal
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.SetupHooks.Internal
import qualified Distribution.Simple.Test as Cabal
import Distribution.Simple.Utils
import Distribution.System (Platform)
import Distribution.Types.BuildType
import Distribution.Types.ComponentRequestedSpec
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Types.LocalBuildInfo
import Distribution.Utils.Path
  ( makeSymbolicPath
  , relativeSymbolicPath
  )

import Distribution.Types.HookedBuildInfo (emptyHookedBuildInfo)
import System.Directory (canonicalizePath)

--------------------------------------------------------------------------------
-- Configure

data LibraryConfigureInputs = LibraryConfigureInputs
  { compiler :: Compiler
  , platform :: Platform
  , buildType :: BuildType
  , compRequested :: Maybe PD.ComponentName
  , localBuildConfig :: LBC.LocalBuildConfig
  , packageDBStack :: PackageDBStackCWD
  , packageDescription :: PD.PackageDescription
  , gPackageDescription :: PD.GenericPackageDescription
  , flagAssignment :: PD.FlagAssignment
  }

libraryConfigureInputsFromElabPackage
  :: BuildType
  -> ProgramDb
  -> ElaboratedSharedConfig
  -> ElaboratedReadyPackage
  -> [String]
  -- ^ targets
  -> LibraryConfigureInputs
libraryConfigureInputsFromElabPackage
  bt
  progDb
  -- NB: don't use the ProgramDb from the ElaboratedSharedConfig;
  -- that one is only for the compiler itself and not for the package.
  ElaboratedSharedConfig
    { pkgConfigPlatform = plat
    , pkgConfigCompiler = compil
    }
  (ReadyPackage pkg)
  userTargets =
    LibraryConfigureInputs
      { compiler = compil
      , platform = plat
      , buildType =
          -- NB: don't get the build-type from 'pkgDescr',
          -- because for Configure build-type we rewrite the build-type
          -- to Simple for components that are neither the main library
          -- nor an executable.
          --
          -- See also 'isMainLibOrExeComponent'.
          bt
      , compRequested =
          case elabPkgOrComp pkg of
            ElabComponent elabComp
              | Just elabCompNm <- compComponentName elabComp ->
                  Just elabCompNm
            _ -> Nothing
      , localBuildConfig =
          LBC.LocalBuildConfig
            { LBC.extraConfigArgs = userTargets
            , LBC.withPrograms = progDb
            , LBC.withBuildOptions = elabBuildOptions pkg
            }
      , packageDBStack = elabBuildPackageDBStack pkg
      , packageDescription = pkgDescr
      , gPackageDescription = gpkgDescr
      , flagAssignment = elabFlagAssignment pkg
      }
    where
      pkgDescr = elabPkgDescription pkg
      gpkgDescr = elabGPkgDescription pkg

configure
  :: LibraryConfigureInputs
  -> Cabal.ConfigFlags
  -> IO LocalBuildInfo
configure
  LibraryConfigureInputs
    { platform = plat
    , compiler = compil
    , buildType = bt
    , compRequested = mbComp
    , localBuildConfig = lbc0
    , packageDBStack = packageDBs
    , packageDescription = pkgDescr
    , gPackageDescription = gpkgDescr
    , flagAssignment = flagAssgn
    }
  cfg = do
    -- Here, we essentially want to call the Cabal library 'configure' function,
    -- but skipping over all the steps we don't need such as rediscovering the
    -- compiler or re-resolving the conditionals in the package, as we have done
    -- all of that already.
    --
    -- To achieve this, we call the Cabal 'configureFinal' function which skips
    -- these preparatory steps.
    -- This code can still be improved, as it seems like 'configureFinal' still
    -- does a fair bit of redundant work. In the end, it would be ideal if the
    -- entirety of this function body was a single call to a function in the
    -- Cabal library that gets called within the Cabal configure function.
    let verbosity = Cabal.fromFlag $ Cabal.configVerbosity cfg
        mbWorkDir = Cabal.flagToMaybe $ Cabal.configWorkingDir cfg
        distPref = Cabal.fromFlag $ Cabal.configDistPref cfg
        confHooks = configureHooks $ ExternalHooksExe.buildTypeSetupHooks mbWorkDir distPref bt

    -- cabal-install uses paths relative to the current working directory,
    -- while the Cabal library expects symbolic paths. Perform the conversion here
    -- by making the paths absolute.
    packageDBs' <- traverse (traverse $ fmap makeSymbolicPath . canonicalizePath) packageDBs

    -- Configure package
    let pkgId :: PD.PackageIdentifier
        pkgId = PD.package pkgDescr
    case mbComp of
      Nothing -> setupMessage verbosity "Configuring" pkgId
      Just cname ->
        setupMessage'
          verbosity
          "Configuring"
          pkgId
          cname
          (Just (Cabal.configInstantiateWith cfg))

    -- TODO: we should avoid re-doing package-wide things over and over
    -- in the per-component world, e.g.
    --   > cabal build comp1 && cabal build comp2
    -- should only run the per-package configuration (including hooks) a single time.
    --
    -- This seemingly requires a rethinking of
    -- Distribution.Client.ProjectBuilding.UnpackedPackage.buildAndRegisterUnpackedPackage
    -- to allow more granular recompilation checking, at the level of components.
    lbc1 <- case preConfPackageHook confHooks of
      Nothing -> return lbc0
      Just hk -> Cabal.runPreConfPackageHook cfg compil plat lbc0 hk
    let compRequestedSpec = case mbComp of
          Just compName -> OneComponentRequestedSpec compName
          Nothing ->
            ComponentRequestedSpec
              { testsRequested = Cabal.fromFlag (Cabal.configTests cfg)
              , benchmarksRequested = Cabal.fromFlag (Cabal.configBenchmarks cfg)
              }
    (_allConstraints, pkgInfo) <-
      Cabal.computePackageInfo cfg lbc1 gpkgDescr compil
    -- NB: no need to re-apply "allConstraints", as we already have a
    -- finalized package description in hand.

    -- Post-configure hooks & per-component configure
    lbi1 <-
      Cabal.configureFinal
        confHooks
        emptyHookedBuildInfo
        cfg
        lbc1
        (gpkgDescr, pkgDescr)
        flagAssgn
        compRequestedSpec
        compil
        plat
        packageDBs'
        pkgInfo

    -- Remember the .cabal filename if we know it.
    pkgDescrFilePath <-
      case Cabal.flagToMaybe $ Cabal.configCabalFilePath cfg of
        Just pkgFile -> return pkgFile
        Nothing -> relativeSymbolicPath <$> tryFindPackageDesc verbosity mbWorkDir
    return $ lbi1{pkgDescrFile = Just pkgDescrFilePath}

--------------------------------------------------------------------------------
-- Build

build
  :: Cabal.BuildFlags
  -> LocalBuildInfo
  -> [String]
  -> IO [MonitorFilePath]
build flags lbi _args =
  build_setupHooks (preBuildHook, postBuildHook) pkgDescr lbi flags Cabal.knownSuffixHandlers
  where
    hooks = ExternalHooksExe.buildTypeSetupHooks mbWorkDir distPref bt
    -- (Recall that pre-build hooks are treated specially;
    -- see the 'buildTypeSetupHooks' and 'buildTypePreBuildHooks' functions.)
    preBuildHook = ExternalHooksExe.buildTypePreBuildHooks mbWorkDir distPref bt
    postBuildHook
      | Just postBuild <- postBuildComponentHook $ buildHooks hooks =
          postBuild
      | otherwise =
          const $ return ()
    pkgDescr = localPkgDescr lbi
    bt = PD.buildType pkgDescr
    mbWorkDir = mbWorkDirLBI lbi
    distPref = Cabal.fromFlag $ Cabal.buildDistPref flags

--------------------------------------------------------------------------------
-- Haddock

haddock
  :: Cabal.HaddockFlags
  -> LocalBuildInfo
  -> [String]
  -> IO [MonitorFilePath]
haddock flags lbi _args =
  haddock_setupHooks preBuildHook pkgDescr lbi Cabal.knownSuffixHandlers flags
  where
    preBuildHook = ExternalHooksExe.buildTypePreBuildHooks mbWorkDir distPref bt
    pkgDescr = localPkgDescr lbi
    bt = PD.buildType pkgDescr
    mbWorkDir = mbWorkDirLBI lbi
    distPref = Cabal.fromFlag $ Cabal.haddockDistPref flags

--------------------------------------------------------------------------------
-- Repl

repl
  :: Cabal.ReplFlags
  -> LocalBuildInfo
  -> [String]
  -> IO ()
repl flags lbi _args =
  repl_setupHooks preBuildHook pkgDescr lbi flags Cabal.knownSuffixHandlers []
  where
    preBuildHook = ExternalHooksExe.buildTypePreBuildHooks mbWorkDir distPref bt
    pkgDescr = localPkgDescr lbi
    bt = PD.buildType pkgDescr
    mbWorkDir = mbWorkDirLBI lbi
    distPref = Cabal.fromFlag $ Cabal.replDistPref flags

--------------------------------------------------------------------------------
-- Copy

copy
  :: Cabal.CopyFlags
  -> LocalBuildInfo
  -> [String]
  -> IO ()
copy flags lbi _args =
  install_setupHooks hooks pkgDescr lbi flags
  where
    hooks = installHooks $ ExternalHooksExe.buildTypeSetupHooks mbWorkDir distPref bt
    pkgDescr = localPkgDescr lbi
    bt = PD.buildType pkgDescr
    mbWorkDir = mbWorkDirLBI lbi
    distPref = Cabal.fromFlag $ Cabal.copyDistPref flags

--------------------------------------------------------------------------------
-- Test, bench, register.
--
-- NB: no hooks into these phases.

test
  :: Cabal.TestFlags
  -> LocalBuildInfo
  -> [String]
  -> IO ()
test flags lbi args =
  Cabal.test args pkgDescr lbi flags
  where
    pkgDescr = localPkgDescr lbi

bench
  :: Cabal.BenchmarkFlags
  -> LocalBuildInfo
  -> [String]
  -> IO ()
bench flags lbi args =
  Cabal.bench args pkgDescr lbi flags
  where
    pkgDescr = localPkgDescr lbi

register
  :: Cabal.RegisterFlags
  -> LocalBuildInfo
  -> [String]
  -> IO ()
register flags lbi _args = Cabal.register pkgDescr lbi flags
  where
    pkgDescr = localPkgDescr lbi
