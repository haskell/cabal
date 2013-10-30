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
-----------------------------------------------------------------------------
module Distribution.Client.Configure (
    configure,
  ) where

import Distribution.Client.Dependency
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan (InstallPlan)
import Distribution.Client.IndexUtils as IndexUtils
         ( getSourcePackages, getInstalledPackages )
import Distribution.Client.Setup
         ( ConfigExFlags(..), configureCommand, filterConfigureFlags )
import Distribution.Client.Types as Source
import Distribution.Client.SetupWrapper
         ( setupWrapper, SetupScriptOptions(..), defaultSetupScriptOptions )
import Distribution.Client.Targets
         ( userToPackageConstraint )

import Distribution.Simple.Compiler
         ( CompilerId(..), Compiler(compilerId)
         , PackageDB(..), PackageDBStack )
import Distribution.Simple.Program (ProgramConfiguration )
import Distribution.Simple.Setup
         ( ConfigFlags(..), fromFlag, toFlag, flagToMaybe, fromFlagOrDefault )
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Simple.Utils
         ( defaultPackageDesc )
import Distribution.Package
         ( Package(..), packageName, Dependency(..), thisPackageVersion )
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.Version
         ( anyVersion, thisVersion )
import Distribution.Simple.Utils as Utils
         ( notice, debug, die )
import Distribution.System
         ( Platform )
import Distribution.Verbosity as Verbosity
         ( Verbosity )

import Data.Monoid (Monoid(..))

-- | Configure the package found in the local directory
configure :: Verbosity
          -> PackageDBStack
          -> [Repo]
          -> Compiler
          -> Platform
          -> ProgramConfiguration
          -> ConfigFlags
          -> ConfigExFlags
          -> [String]
          -> IO ()
configure verbosity packageDBs repos comp platform conf
  configFlags configExFlags extraArgs = do

  installedPkgIndex <- getInstalledPackages verbosity comp packageDBs conf
  sourcePkgDb       <- getSourcePackages    verbosity repos

  progress <- planLocalPackage verbosity comp platform configFlags configExFlags
                               installedPkgIndex sourcePkgDb

  notice verbosity "Resolving dependencies..."
  maybePlan <- foldProgress logMsg (return . Left) (return . Right)
                            progress
  case maybePlan of
    Left message -> die message

    Right installPlan -> case InstallPlan.ready installPlan of
      [(pkg@(ConfiguredPackage (SourcePackage _ _ (LocalUnpackedPackage _) _) _ _ _), _)] ->
        configurePackage verbosity
          (InstallPlan.planPlatform installPlan)
          (InstallPlan.planCompiler installPlan)
          (setupScriptOptions installedPkgIndex)
          configFlags pkg extraArgs

      _ -> die $ "internal error: configure install plan should have exactly "
              ++ "one local ready package."

  where
    setupScriptOptions index = SetupScriptOptions {
      useCabalVersion  = maybe anyVersion thisVersion
                         (flagToMaybe (configCabalVersion configExFlags)),
      useCompiler      = Just comp,
      usePlatform      = Just platform,
      usePackageDB     = packageDBs',
      usePackageIndex  = index',
      useProgramConfig = conf,
      useDistPref      = fromFlagOrDefault
                           (useDistPref defaultSetupScriptOptions)
                           (configDistPref configFlags),
      useLoggingHandle = Nothing,
      useWorkingDir    = Nothing,
      forceExternalSetupMethod = False,
      setupCacheLock   = Nothing
    }
      where
        -- Hack: we typically want to allow the UserPackageDB for finding the
        -- Cabal lib when compiling any Setup.hs even if we're doing a global
        -- install. However we also allow looking in a specific package db.
        (packageDBs', index') =
          case packageDBs of
            (GlobalPackageDB:dbs) | UserPackageDB `notElem` dbs
                -> (GlobalPackageDB:UserPackageDB:dbs, Nothing)
            -- but if the user is using an odd db stack, don't touch it
            dbs -> (dbs, Just index)

    logMsg message rest = debug verbosity message >> rest

-- | Make an 'InstallPlan' for the unpacked package in the current directory,
-- and all its dependencies.
--
planLocalPackage :: Verbosity -> Compiler
                 -> Platform
                 -> ConfigFlags -> ConfigExFlags
                 -> PackageIndex
                 -> SourcePackageDb
                 -> IO (Progress String String InstallPlan)
planLocalPackage verbosity comp platform configFlags configExFlags installedPkgIndex
  (SourcePackageDb _ packagePrefs) = do
  pkg <- readPackageDescription verbosity =<< defaultPackageDesc verbosity
  solver <- chooseSolver verbosity (fromFlag $ configSolver configExFlags) (compilerId comp)

  let -- We create a local package and ask to resolve a dependency on it
      localPkg = SourcePackage {
        packageInfoId             = packageId pkg,
        Source.packageDescription = pkg,
        packageSource             = LocalUnpackedPackage ".",
        packageDescrOverride      = Nothing
      }

      testsEnabled = fromFlagOrDefault False $ configTests configFlags
      benchmarksEnabled =
        fromFlagOrDefault False $ configBenchmarks configFlags

      resolverParams =

          addPreferences
            -- preferences from the config file or command line
            [ PackageVersionPreference name ver
            | Dependency name ver <- configPreferences configExFlags ]

        . addConstraints
            -- version constraints from the config file or command line
            -- TODO: should warn or error on constraints that are not on direct deps
            -- or flag constraints not on the package in question.
            (map userToPackageConstraint (configExConstraints configExFlags))

        . addConstraints
            -- package flags from the config file or command line
            [ PackageConstraintFlags (packageName pkg)
                                     (configConfigurationsFlags configFlags) ]

        . addConstraints
            -- '--enable-tests' and '--enable-benchmarks' constraints from
            -- command line
            [ PackageConstraintStanzas (packageName pkg) $
                [ TestStanzas  | testsEnabled ] ++
                [ BenchStanzas | benchmarksEnabled ]
            ]

        $ standardInstallPolicy
            installedPkgIndex
            (SourcePackageDb mempty packagePrefs)
            [SpecificSourcePackage localPkg]

  return (resolveDependencies platform (compilerId comp) solver resolverParams)


-- | Call an installer for an 'SourcePackage' but override the configure
-- flags with the ones given by the 'ConfiguredPackage'. In particular the
-- 'ConfiguredPackage' specifies an exact 'FlagAssignment' and exactly
-- versioned package dependencies. So we ignore any previous partial flag
-- assignment or dependency constraints and use the new ones.
--
configurePackage :: Verbosity
                 -> Platform -> CompilerId
                 -> SetupScriptOptions
                 -> ConfigFlags
                 -> ConfiguredPackage
                 -> [String]
                 -> IO ()
configurePackage verbosity platform comp scriptOptions configFlags
  (ConfiguredPackage (SourcePackage _ gpkg _ _) flags stanzas deps) extraArgs =

  setupWrapper verbosity
    scriptOptions (Just pkg) configureCommand configureFlags extraArgs

  where
    configureFlags   = filterConfigureFlags configFlags {
      configConfigurationsFlags = flags,
      configConstraints         = map thisPackageVersion deps,
      configVerbosity           = toFlag verbosity,
      configBenchmarks          = toFlag (BenchStanzas `elem` stanzas),
      configTests               = toFlag (TestStanzas `elem` stanzas)
    }

    pkg = case finalizePackageDescription flags
           (const True)
           platform comp [] (enableStanzas stanzas gpkg) of
      Left _ -> error "finalizePackageDescription ConfiguredPackage failed"
      Right (desc, _) -> desc
