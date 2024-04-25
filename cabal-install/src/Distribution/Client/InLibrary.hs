{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

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
import Distribution.Simple (Compiler, PackageDBStack)
import qualified Distribution.Simple.Bench as Cabal
import Distribution.Simple.Build (build_setupHooks, repl_setupHooks)
import qualified Distribution.Simple.Configure as Cabal
import Distribution.Simple.Haddock (haddock_setupHooks)
import Distribution.Simple.Install (install_setupHooks)
import Distribution.Simple.LocalBuildInfo
  ( Component
  , componentName
  , mbWorkDirLBI
  )
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
  ( relativeSymbolicPath
  )

import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Configure

data LibraryConfigureInputs = LibraryConfigureInputs
  { compiler :: Compiler
  , platform :: Platform
  , buildType :: BuildType
  , compRequested :: Maybe PD.ComponentName
  , localBuildConfig :: LBC.LocalBuildConfig
  , packageDBStack :: PackageDBStack
  , packageDescription :: PD.PackageDescription
  , gPackageDescription :: PD.GenericPackageDescription
  , flagAssignment :: PD.FlagAssignment
  }

libraryConfigureInputsFromElabPackage
  :: ProgramDb
  -> ElaboratedSharedConfig
  -> ElaboratedReadyPackage
  -> [String]
  -- ^ targets
  -> LibraryConfigureInputs
libraryConfigureInputsFromElabPackage
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
      , buildType = PD.buildType pkgDescr
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
    , packageDescription = pkgDesc
    , gPackageDescription = gpkgDescr
    , flagAssignment = flagAssgn
    }
  cfg =
    -- TODO: the following code should not live in cabal-install.
    -- We should be able to directly call into the library,
    -- similar to what we do for other phases (see e.g. inLibraryBuild).
    --
    -- The difficulty is that we start off at a rather different place, having
    -- already some configuration information in hand (we have a compiler and
    -- a PackageDescription with resolved conditionals, for instance).
    --
    -- It would be worthwhile to refactor the Cabal library 'configure'
    -- functions so that as little logic exists in cabal-install as possible.
    do
      let verbosity = Cabal.fromFlag $ Cabal.configVerbosity cfg
          mbWorkDir = Cabal.flagToMaybe $ Cabal.configWorkingDir cfg
          distPref = Cabal.fromFlag $ Cabal.configDistPref cfg
          confHooks = configureHooks $ ExternalHooksExe.buildTypeSetupHooks mbWorkDir distPref bt

      -- Configure package
      let pkgId :: PD.PackageIdentifier
          pkgId = PD.package pkgDesc
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
      let compRequested = case mbComp of
            Just compName -> OneComponentRequestedSpec compName
            Nothing ->
              ComponentRequestedSpec
                { testsRequested = Cabal.fromFlag (Cabal.configTests cfg)
                , benchmarksRequested = Cabal.fromFlag (Cabal.configBenchmarks cfg)
                }
      (lbc2, pbd2) <-
        Cabal.configurePackage
          cfg
          lbc1
          pkgDesc
          flagAssgn
          compRequested
          compil
          plat
          packageDBs
      for_ (postConfPackageHook confHooks) $ Cabal.runPostConfPackageHook lbc2 pbd2
      let pkg_descr2 = LBC.localPkgDescr pbd2

      -- Configure component(s)
      pkg_descr <-
        applyComponentDiffs
          verbosity
          ( \comp ->
              if wantComponent compRequested comp
                then traverse (Cabal.runPreConfComponentHook lbc2 pbd2 comp) $ preConfComponentHook confHooks
                else return Nothing
          )
          pkg_descr2
      let pbd3 = pbd2{LBC.localPkgDescr = pkg_descr}

      -- Emit any errors/warnings on problems in the .cabal file.
      --
      -- TODO: it might make sense to move this check earlier, perhaps somewhere
      -- in Distribution.Client.ProjectPlanning.elaborateInstallPlan.
      -- See ticket #9995 for more information.
      Cabal.finalCheckPackage gpkgDescr pbd3 PD.emptyHookedBuildInfo

      -- This is more logic that we would like to have in cabal-install
      -- (see the TODO at the top of this function for more comments).
      let progdb = LBC.withPrograms lbc2
          promisedDeps = Cabal.mkPromisedDepsSet (Cabal.configPromisedDependencies cfg)
      installedPkgSet <- Cabal.getInstalledPackages verbosity compil mbWorkDir packageDBs progdb
      (_, depsMap) <-
        either (dieWithException verbosity) return $
          Cabal.combinedConstraints
            (Cabal.configConstraints cfg)
            (Cabal.configDependencies cfg)
            installedPkgSet
      let pkg_info =
            Cabal.PackageInfo
              { internalPackageSet = Set.fromList (map PD.libName (PD.allLibraries pkg_descr))
              , promisedDepsSet = promisedDeps
              , installedPackageSet = installedPkgSet
              , requiredDepsMap = depsMap
              }
          useExternalInternalDeps = case compRequested of
            OneComponentRequestedSpec{} -> True
            ComponentRequestedSpec{} -> False
      externalPkgDeps <- Cabal.configureDependencies verbosity useExternalInternalDeps pkg_info pkg_descr compRequested
      lbi1 <- Cabal.configureComponents lbc2 pbd3 installedPkgSet promisedDeps externalPkgDeps

      pkgDescrFilePath <-
        case Cabal.flagToMaybe $ Cabal.configCabalFilePath cfg of
          Just pkgFile -> return pkgFile
          Nothing -> relativeSymbolicPath <$> tryFindPackageDesc verbosity mbWorkDir
      let lbi2 = lbi1{pkgDescrFile = Just pkgDescrFilePath}
      return lbi2

-- NB: this function might match multiple components,
-- due to Backpack instantiations.
wantComponent :: ComponentRequestedSpec -> Component -> Bool
wantComponent compReq comp = case compReq of
  ComponentRequestedSpec{} -> True
  OneComponentRequestedSpec reqComp ->
    componentName comp == reqComp

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
