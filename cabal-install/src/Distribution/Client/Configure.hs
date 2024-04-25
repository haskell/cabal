{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Configure
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Duncan Coutts 2005
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- High level interface to configuring a package.
module Distribution.Client.Configure
  ( configure
  , configureSetupScript
  , chooseCabalVersion
  , checkConfigExFlags

    -- * Saved configure flags
  , readConfigFlagsFrom
  , readConfigFlags
  , writeConfigFlagsTo
  , writeConfigFlags
  ) where

import Distribution.Client.Compat.Prelude
import Distribution.Utils.Generic (safeHead)
import Prelude ()

import Distribution.Client.Dependency
import Distribution.Client.IndexUtils as IndexUtils
  ( getInstalledPackages
  , getSourcePackages
  )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.JobControl (Lock)
import Distribution.Client.Setup
  ( ConfigExFlags (..)
  , RepoContext (..)
  , configureCommand
  , configureExCommand
  , filterConfigureFlags
  )
import Distribution.Client.SetupWrapper
  ( SetupRunnerArgs (NotInLibrary)
  , SetupScriptOptions (..)
  , defaultSetupScriptOptions
  , setupWrapper
  )
import Distribution.Client.SolverInstallPlan (SolverInstallPlan)
import Distribution.Client.Targets
  ( userConstraintPackageName
  , userToPackageConstraint
  )
import Distribution.Client.Types as Source

import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackageIndex
  ( PackageIndex
  , elemByPackageName
  )
import Distribution.Solver.Types.PkgConfigDb
  ( PkgConfigDb
  , readPkgConfigDb
  )
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.SourcePackage

import Distribution.Client.SavedFlags (readCommandFlags, writeCommandFlags)
import Distribution.Package
  ( Package (..)
  , PackageId
  , packageName
  )
import qualified Distribution.PackageDescription as PkgDesc
import Distribution.PackageDescription.Configuration
  ( finalizePD
  )
import Distribution.Simple.Compiler
  ( Compiler
  , CompilerInfo
  , PackageDB (..)
  , PackageDBStack
  , compilerInfo
  )
import Distribution.Simple.PackageDescription
  ( readGenericPackageDescription
  )
import Distribution.Simple.PackageIndex as PackageIndex
  ( InstalledPackageIndex
  , lookupPackageName
  )
import Distribution.Simple.Program (ProgramDb)
import Distribution.Simple.Setup
  ( CommonSetupFlags (..)
  , ConfigFlags (..)
  , flagToMaybe
  , fromFlagOrDefault
  , maybeToFlag
  , toFlag
  )
import Distribution.Simple.Utils as Utils
  ( debug
  , defaultPackageDescCwd
  , dieWithException
  , notice
  , warn
  )
import Distribution.System
  ( Platform
  )
import Distribution.Types.GivenComponent
  ( GivenComponent (..)
  )
import Distribution.Types.PackageVersionConstraint
  ( PackageVersionConstraint (..)
  , thisPackageVersionConstraint
  )
import Distribution.Utils.Path
import Distribution.Version
  ( Version
  , VersionRange
  , anyVersion
  , thisVersion
  )

import Distribution.Client.Errors

-- | Choose the Cabal version such that the setup scripts compiled against this
-- version will support the given command-line flags. Currently, it implements no
-- specific restrictions and allows any version, unless the second argument is
-- filled with a 'Version', in which case this version is picked.
chooseCabalVersion :: ConfigExFlags -> Maybe Version -> VersionRange
chooseCabalVersion _configExFlags maybeVersion =
  maybe anyVersion thisVersion maybeVersion

-- | Configure the package found in the local directory
configure
  :: Verbosity
  -> PackageDBStack
  -> RepoContext
  -> Compiler
  -> Platform
  -> ProgramDb
  -> ConfigFlags
  -> ConfigExFlags
  -> [String]
  -> IO ()
configure
  verbosity
  packageDBs
  repoCtxt
  comp
  platform
  progdb
  configFlags
  configExFlags
  extraArgs = do
    installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
    sourcePkgDb <- getSourcePackages verbosity repoCtxt
    pkgConfigDb <- readPkgConfigDb verbosity progdb

    checkConfigExFlags
      verbosity
      installedPkgIndex
      (packageIndex sourcePkgDb)
      configExFlags

    progress <-
      planLocalPackage
        verbosity
        comp
        platform
        configFlags
        configExFlags
        installedPkgIndex
        sourcePkgDb
        pkgConfigDb

    notice verbosity "Resolving dependencies..."
    maybePlan <-
      foldProgress
        logMsg
        (return . Left)
        (return . Right)
        progress
    case maybePlan of
      Left message -> do
        warn verbosity $
          "solver failed to find a solution:\n"
            ++ message
            ++ "\nTrying configure anyway."
        setupWrapper
          verbosity
          (setupScriptOptions installedPkgIndex Nothing)
          Nothing
          configureCommand
          configCommonFlags
          (const configFlags)
          (const extraArgs)
          NotInLibrary
      Right installPlan0 ->
        let installPlan = InstallPlan.configureInstallPlan configFlags installPlan0
         in case fst (InstallPlan.ready installPlan) of
              [ pkg@( ReadyPackage
                        ( ConfiguredPackage
                            _
                            (SourcePackage _ _ (LocalUnpackedPackage _) _)
                            _
                            _
                            _
                          )
                      )
                ] -> do
                  configurePackage
                    verbosity
                    platform
                    (compilerInfo comp)
                    (setupScriptOptions installedPkgIndex (Just pkg))
                    configFlags
                    pkg
                    extraArgs
              _ ->
                dieWithException verbosity ConfigureInstallInternalError
    where
      setupScriptOptions
        :: InstalledPackageIndex
        -> Maybe ReadyPackage
        -> SetupScriptOptions
      setupScriptOptions =
        configureSetupScript
          packageDBs
          comp
          platform
          progdb
          ( fromFlagOrDefault
              (useDistPref defaultSetupScriptOptions)
              (setupDistPref $ configCommonFlags configFlags)
          )
          ( chooseCabalVersion
              configExFlags
              (flagToMaybe (configCabalVersion configExFlags))
          )
          Nothing

      logMsg message rest = debug verbosity message >> rest

configureSetupScript
  :: PackageDBStack
  -> Compiler
  -> Platform
  -> ProgramDb
  -> SymbolicPath Pkg (Dir Dist)
  -> VersionRange
  -> Maybe Lock
  -> InstalledPackageIndex
  -> Maybe ReadyPackage
  -> SetupScriptOptions
configureSetupScript
  packageDBs
  comp
  platform
  progdb
  distPref
  cabalVersion
  lock
  index
  mpkg =
    SetupScriptOptions
      { useCabalVersion = cabalVersion
      , useCabalSpecVersion = Nothing
      , useCompiler = Just comp
      , usePlatform = Just platform
      , usePackageDB = packageDBs'
      , usePackageIndex = index'
      , useProgramDb = progdb
      , useDistPref = distPref
      , useLoggingHandle = Nothing
      , useWorkingDir = Nothing
      , useExtraPathEnv = []
      , useExtraEnvOverrides = []
      , setupCacheLock = lock
      , useWin32CleanHack = False
      , -- If we have explicit setup dependencies, list them; otherwise, we give
        -- the empty list of dependencies; ideally, we would fix the version of
        -- Cabal here, so that we no longer need the special case for that in
        -- `compileSetupExecutable` in `externalSetupMethod`, but we don't yet
        -- know the version of Cabal at this point, but only find this there.
        -- Therefore, for now, we just leave this blank.
        useDependencies = fromMaybe [] explicitSetupDeps
      , useDependenciesExclusive = not defaultSetupDeps && isJust explicitSetupDeps
      , useVersionMacros = not defaultSetupDeps && isJust explicitSetupDeps
      , isInteractive = False
      }
    where
      -- When we are compiling a legacy setup script without an explicit
      -- setup stanza, we typically want to allow the UserPackageDB for
      -- finding the Cabal lib when compiling any Setup.hs even if we're doing
      -- a global install. However we also allow looking in a specific package
      -- db.
      packageDBs' :: PackageDBStack
      index' :: Maybe InstalledPackageIndex
      (packageDBs', index') =
        case packageDBs of
          (GlobalPackageDB : dbs)
            | UserPackageDB `notElem` dbs
            , Nothing <- explicitSetupDeps ->
                (GlobalPackageDB : UserPackageDB : dbs, Nothing)
          -- but if the user is using an odd db stack, don't touch it
          _otherwise -> (packageDBs, Just index)

      maybeSetupBuildInfo :: Maybe PkgDesc.SetupBuildInfo
      maybeSetupBuildInfo = do
        ReadyPackage cpkg <- mpkg
        let gpkg = srcpkgDescription (confPkgSource cpkg)
        PkgDesc.setupBuildInfo (PkgDesc.packageDescription gpkg)

      -- Was a default 'custom-setup' stanza added by 'cabal-install' itself? If
      -- so, 'setup-depends' must not be exclusive. See #3199.
      defaultSetupDeps :: Bool
      defaultSetupDeps =
        maybe
          False
          PkgDesc.defaultSetupDepends
          maybeSetupBuildInfo

      explicitSetupDeps :: Maybe [(InstalledPackageId, PackageId)]
      explicitSetupDeps = do
        -- Check if there is an explicit setup stanza.
        _buildInfo <- maybeSetupBuildInfo
        -- Return the setup dependencies computed by the solver
        ReadyPackage cpkg <- mpkg
        return
          [ (cid, srcid)
          | ConfiguredId
              srcid
              (Just (PkgDesc.CLibName PkgDesc.LMainLibName))
              cid <-
              CD.setupDeps (confPkgDeps cpkg)
          ]

-- | Warn if any constraints or preferences name packages that are not in the
-- source package index or installed package index.
checkConfigExFlags
  :: Package pkg
  => Verbosity
  -> InstalledPackageIndex
  -> PackageIndex pkg
  -> ConfigExFlags
  -> IO ()
checkConfigExFlags verbosity installedPkgIndex sourcePkgIndex flags = do
  for_ (safeHead unknownConstraints) $ \h ->
    warn verbosity $
      "Constraint refers to an unknown package: "
        ++ showConstraint h
  for_ (safeHead unknownPreferences) $ \h ->
    warn verbosity $
      "Preference refers to an unknown package: "
        ++ prettyShow h
  where
    unknownConstraints =
      filter (unknown . userConstraintPackageName . fst) $
        configExConstraints flags
    unknownPreferences =
      filter (unknown . \(PackageVersionConstraint name _) -> name) $
        configPreferences flags
    unknown pkg =
      null (PackageIndex.lookupPackageName installedPkgIndex pkg)
        && not (elemByPackageName sourcePkgIndex pkg)
    showConstraint (uc, src) =
      prettyShow uc ++ " (" ++ showConstraintSource src ++ ")"

-- | Make an 'InstallPlan' for the unpacked package in the current directory,
-- and all its dependencies.
planLocalPackage
  :: Verbosity
  -> Compiler
  -> Platform
  -> ConfigFlags
  -> ConfigExFlags
  -> InstalledPackageIndex
  -> SourcePackageDb
  -> PkgConfigDb
  -> IO (Progress String String SolverInstallPlan)
planLocalPackage
  verbosity
  comp
  platform
  configFlags
  configExFlags
  installedPkgIndex
  (SourcePackageDb _ packagePrefs)
  pkgConfigDb = do
    pkg <-
      readGenericPackageDescription verbosity Nothing
        =<< case flagToMaybe (setupCabalFilePath $ configCommonFlags configFlags) of
          Nothing -> relativeSymbolicPath <$> defaultPackageDescCwd verbosity
          Just fp -> return fp

    let
      -- We create a local package and ask to resolve a dependency on it
      localPkg =
        SourcePackage
          { srcpkgPackageId = packageId pkg
          , srcpkgDescription = pkg
          , srcpkgSource = LocalUnpackedPackage "."
          , srcpkgDescrOverride = Nothing
          }

      testsEnabled :: Bool
      testsEnabled = fromFlagOrDefault False $ configTests configFlags
      benchmarksEnabled :: Bool
      benchmarksEnabled =
        fromFlagOrDefault False $ configBenchmarks configFlags

      resolverParams :: DepResolverParams
      resolverParams =
        removeLowerBounds
          (fromMaybe (AllowOlder mempty) $ configAllowOlder configExFlags)
          . removeUpperBounds
            (fromMaybe (AllowNewer mempty) $ configAllowNewer configExFlags)
          . addPreferences
            -- preferences from the config file or command line
            [ PackageVersionPreference name ver
            | PackageVersionConstraint name ver <- configPreferences configExFlags
            ]
          . addConstraints
            -- version constraints from the config file or command line
            -- TODO: should warn or error on constraints that are not on direct
            -- deps or flag constraints not on the package in question.
            [ LabeledPackageConstraint (userToPackageConstraint uc) src
            | (uc, src) <- configExConstraints configExFlags
            ]
          . addConstraints
            -- package flags from the config file or command line
            [ let pc =
                    PackageConstraint
                      (scopeToplevel $ packageName pkg)
                      (PackagePropertyFlags $ configConfigurationsFlags configFlags)
               in LabeledPackageConstraint pc ConstraintSourceConfigFlagOrTarget
            ]
          . addConstraints
            -- '--enable-tests' and '--enable-benchmarks' constraints from
            -- the config file or command line
            [ let pc =
                    PackageConstraint (scopeToplevel $ packageName pkg)
                      . PackagePropertyStanzas
                      $ [TestStanzas | testsEnabled]
                        ++ [BenchStanzas | benchmarksEnabled]
               in LabeledPackageConstraint pc ConstraintSourceConfigFlagOrTarget
            ]
          -- Don't solve for executables, since we use an empty source
          -- package database and executables never show up in the
          -- installed package index
          . setSolveExecutables (SolveExecutables False)
          . setSolverVerbosity verbosity
          $ standardInstallPolicy
            installedPkgIndex
            -- NB: We pass in an *empty* source package database,
            -- because cabal configure assumes that all dependencies
            -- have already been installed
            (SourcePackageDb mempty packagePrefs)
            [SpecificSourcePackage localPkg]

    return (resolveDependencies platform (compilerInfo comp) pkgConfigDb resolverParams)

-- | Call an installer for an 'SourcePackage' but override the configure
-- flags with the ones given by the 'ReadyPackage'. In particular the
-- 'ReadyPackage' specifies an exact 'FlagAssignment' and exactly
-- versioned package dependencies. So we ignore any previous partial flag
-- assignment or dependency constraints and use the new ones.
--
-- NB: when updating this function, don't forget to also update
-- 'installReadyPackage' in D.C.Install.
configurePackage
  :: Verbosity
  -> Platform
  -> CompilerInfo
  -> SetupScriptOptions
  -> ConfigFlags
  -> ReadyPackage
  -> [String]
  -> IO ()
configurePackage
  verbosity
  platform
  comp
  scriptOptions
  configFlags
  (ReadyPackage (ConfiguredPackage ipid spkg flags stanzas deps))
  extraArgs =
    setupWrapper
      verbosity
      scriptOptions
      (Just pkg)
      configureCommand
      configCommonFlags
      configureFlags
      (const extraArgs)
      NotInLibrary
    where
      gpkg :: PkgDesc.GenericPackageDescription
      gpkg = srcpkgDescription spkg
      configureFlags :: Version -> ConfigFlags
      configureFlags =
        filterConfigureFlags
          configFlags
            { configCommonFlags =
                (configCommonFlags configFlags)
                  { setupVerbosity = toFlag verbosity
                  , setupWorkingDir = maybeToFlag $ useWorkingDir scriptOptions
                  }
            , configIPID =
                if isJust (flagToMaybe (configIPID configFlags))
                  then -- Make sure cabal configure --ipid works.
                    configIPID configFlags
                  else toFlag (prettyShow ipid)
            , configConfigurationsFlags = flags
            , -- We generate the legacy constraints as well as the new style precise
              -- deps.  In the end only one set gets passed to Setup.hs configure,
              -- depending on the Cabal version we are talking to.
              configConstraints =
                [ thisPackageVersionConstraint srcid
                | ConfiguredId srcid (Just (PkgDesc.CLibName PkgDesc.LMainLibName)) _uid <-
                    CD.nonSetupDeps deps
                ]
            , configDependencies =
                [ GivenComponent (packageName srcid) cname uid
                | ConfiguredId srcid (Just (PkgDesc.CLibName cname)) uid <-
                    CD.nonSetupDeps deps
                ]
            , -- Use '--exact-configuration' if supported.
              configExactConfiguration = toFlag True
            , -- NB: if the user explicitly specified
              -- --enable-tests/--enable-benchmarks, always respect it.
              -- (But if they didn't, let solver decide.)
              configBenchmarks =
                toFlag (BenchStanzas `optStanzaSetMember` stanzas)
                  `mappend` configBenchmarks configFlags
            , configTests =
                toFlag (TestStanzas `optStanzaSetMember` stanzas)
                  `mappend` configTests configFlags
            }

      pkg :: PkgDesc.PackageDescription
      pkg = case finalizePD
        flags
        (enableStanzas stanzas)
        (const True)
        platform
        comp
        []
        gpkg of
        Left _ -> error "finalizePD ReadyPackage failed"
        Right (desc, _) -> desc

-- -----------------------------------------------------------------------------

-- * Saved configure environments and flags

-- -----------------------------------------------------------------------------

-- | Read saved configure flags and restore the saved environment from the
-- specified files.
readConfigFlagsFrom
  :: FilePath
  -- ^ path to saved flags file
  -> IO (ConfigFlags, ConfigExFlags)
readConfigFlagsFrom flags = do
  readCommandFlags flags configureExCommand

-- | The path (relative to the package root) where the arguments to @configure@
-- should be saved.
cabalConfigFlagsFile :: FilePath -> FilePath
cabalConfigFlagsFile dist = dist </> "cabal-config-flags"

-- | Read saved configure flags and restore the saved environment from the
-- usual location.
readConfigFlags
  :: FilePath
  -- ^ @--build-dir@
  -> IO (ConfigFlags, ConfigExFlags)
readConfigFlags dist =
  readConfigFlagsFrom (cabalConfigFlagsFile dist)

-- | Save the configure flags and environment to the specified files.
writeConfigFlagsTo
  :: Verbosity
  -> FilePath
  -- ^ path to saved flags file
  -> (ConfigFlags, ConfigExFlags)
  -> IO ()
writeConfigFlagsTo verb file flags = do
  writeCommandFlags verb file configureExCommand flags

-- | Save the build flags to the usual location.
writeConfigFlags
  :: Verbosity
  -> FilePath
  -- ^ @--build-dir@
  -> (ConfigFlags, ConfigExFlags)
  -> IO ()
writeConfigFlags verb dist =
  writeConfigFlagsTo verb (cabalConfigFlagsFile dist)
