{-# LANGUAGE CPP #-}
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
    configureSetupScript,
    chooseCabalVersion,
  ) where

import Distribution.Client.Dependency
import Distribution.Client.Dependency.Types
         ( AllowNewer(..), isAllowNewer, ConstraintSource(..)
         , LabeledPackageConstraint(..) )
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
import qualified Distribution.Client.ComponentDeps as CD
import Distribution.Package (PackageId)
import Distribution.Client.JobControl (Lock)

import Distribution.Simple.Compiler
         ( Compiler, CompilerInfo, compilerInfo, PackageDB(..), PackageDBStack )
import Distribution.Simple.Program (ProgramConfiguration )
import Distribution.Simple.Setup
         ( ConfigFlags(..), fromFlag, toFlag, flagToMaybe, fromFlagOrDefault )
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Utils
         ( defaultPackageDesc )
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.Package
         ( Package(..), InstalledPackageId, packageName
         , Dependency(..), thisPackageVersion
         )
import qualified Distribution.PackageDescription as PkgDesc
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.Version
         ( anyVersion, thisVersion )
import Distribution.Simple.Utils as Utils
         ( notice, info, debug, die )
import Distribution.System
         ( Platform )
import Distribution.Verbosity as Verbosity
         ( Verbosity )
import Distribution.Version
         ( Version(..), VersionRange, orLaterVersion )

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid(..))
#endif
import Data.Maybe (isJust, fromMaybe)

-- | Choose the Cabal version such that the setup scripts compiled against this
-- version will support the given command-line flags.
chooseCabalVersion :: ConfigExFlags -> Maybe Version -> VersionRange
chooseCabalVersion configExFlags maybeVersion =
  maybe defaultVersionRange thisVersion maybeVersion
  where
    -- Cabal < 1.19.2 doesn't support '--exact-configuration' which is needed
    -- for '--allow-newer' to work.
    allowNewer = fromFlagOrDefault False $
                 fmap isAllowNewer (configAllowNewer configExFlags)

    defaultVersionRange = if allowNewer
                          then orLaterVersion (Version [1,19,2] [])
                          else anyVersion

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
    Left message -> do
      info verbosity $
           "Warning: solver failed to find a solution:\n"
        ++ message
        ++ "Trying configure anyway."
      setupWrapper verbosity (setupScriptOptions installedPkgIndex Nothing)
        Nothing configureCommand (const configFlags) extraArgs

    Right installPlan -> case InstallPlan.ready installPlan of
      [pkg@(ReadyPackage
             (ConfiguredPackage (SourcePackage _ _ (LocalUnpackedPackage _) _)
                                 _ _ _)
             _)] -> do
        configurePackage verbosity
          platform (compilerInfo comp)
          (setupScriptOptions installedPkgIndex (Just pkg))
          configFlags pkg extraArgs

      _ -> die $ "internal error: configure install plan should have exactly "
              ++ "one local ready package."

  where
    setupScriptOptions :: InstalledPackageIndex
                       -> Maybe ReadyPackage
                       -> SetupScriptOptions
    setupScriptOptions =
      configureSetupScript
        packageDBs
        comp
        platform
        conf
        (fromFlagOrDefault
           (useDistPref defaultSetupScriptOptions)
           (configDistPref configFlags))
        (chooseCabalVersion
           configExFlags
           (flagToMaybe (configCabalVersion configExFlags)))
        Nothing
        False

    logMsg message rest = debug verbosity message >> rest

configureSetupScript :: PackageDBStack
                     -> Compiler
                     -> Platform
                     -> ProgramConfiguration
                     -> FilePath
                     -> VersionRange
                     -> Maybe Lock
                     -> Bool
                     -> InstalledPackageIndex
                     -> Maybe ReadyPackage
                     -> SetupScriptOptions
configureSetupScript packageDBs
                     comp
                     platform
                     conf
                     distPref
                     cabalVersion
                     lock
                     forceExternal
                     index
                     mpkg
  = SetupScriptOptions {
      useCabalVersion   = cabalVersion
    , useCompiler       = Just comp
    , usePlatform       = Just platform
    , usePackageDB      = packageDBs'
    , usePackageIndex   = index'
    , useProgramConfig  = conf
    , useDistPref       = distPref
    , useLoggingHandle  = Nothing
    , useWorkingDir     = Nothing
    , setupCacheLock    = lock
    , useWin32CleanHack = False
    , forceExternalSetupMethod = forceExternal
      -- If we have explicit setup dependencies, list them; otherwise, we give
      -- the empty list of dependencies; ideally, we would fix the version of
      -- Cabal here, so that we no longer need the special case for that in
      -- `compileSetupExecutable` in `externalSetupMethod`, but we don't yet
      -- know the version of Cabal at this point, but only find this there.
      -- Therefore, for now, we just leave this blank.
    , useDependencies          = fromMaybe [] explicitSetupDeps
    , useDependenciesExclusive = isJust explicitSetupDeps
    }
  where
    -- When we are compiling a legacy setup script without an explicit
    -- setup stanza, we typically want to allow the UserPackageDB for
    -- finding the Cabal lib when compiling any Setup.hs even if we're doing
    -- a global install. However we also allow looking in a specific package
    -- db.
    packageDBs' :: PackageDBStack
    index'      :: Maybe InstalledPackageIndex
    (packageDBs', index') =
      case packageDBs of
        (GlobalPackageDB:dbs) | UserPackageDB `notElem` dbs
                              , Nothing <- explicitSetupDeps
            -> (GlobalPackageDB:UserPackageDB:dbs, Nothing)
        -- but if the user is using an odd db stack, don't touch it
        _otherwise -> (packageDBs, Just index)

    explicitSetupDeps :: Maybe [(InstalledPackageId, PackageId)]
    explicitSetupDeps = do
      ReadyPackage (ConfiguredPackage (SourcePackage _ gpkg _ _) _ _ _) deps
                 <- mpkg
      -- Check if there is an explicit setup stanza
      _buildInfo <- PkgDesc.setupBuildInfo (PkgDesc.packageDescription gpkg)
      -- Return the setup dependencies computed by the solver
      return [ ( Installed.installedPackageId deppkg
               , Installed.sourcePackageId    deppkg
               )
             | deppkg <- CD.setupDeps deps
             ]

-- | Make an 'InstallPlan' for the unpacked package in the current directory,
-- and all its dependencies.
--
planLocalPackage :: Verbosity -> Compiler
                 -> Platform
                 -> ConfigFlags -> ConfigExFlags
                 -> InstalledPackageIndex
                 -> SourcePackageDb
                 -> IO (Progress String String InstallPlan)
planLocalPackage verbosity comp platform configFlags configExFlags
  installedPkgIndex
  (SourcePackageDb _ packagePrefs) = do
  pkg <- readPackageDescription verbosity =<< defaultPackageDesc verbosity
  solver <- chooseSolver verbosity (fromFlag $ configSolver configExFlags)
            (compilerInfo comp)

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
          removeUpperBounds (fromFlagOrDefault AllowNewerNone $
                             configAllowNewer configExFlags)

        . addPreferences
            -- preferences from the config file or command line
            [ PackageVersionPreference name ver
            | Dependency name ver <- configPreferences configExFlags ]

        . addConstraints
            -- version constraints from the config file or command line
            -- TODO: should warn or error on constraints that are not on direct
            -- deps or flag constraints not on the package in question.
            [ LabeledPackageConstraint (userToPackageConstraint uc) src
            | (uc, src) <- configExConstraints configExFlags ]

        . addConstraints
            -- package flags from the config file or command line
            [ let pc = PackageConstraintFlags (packageName pkg)
                       (configConfigurationsFlags configFlags)
              in LabeledPackageConstraint pc ConstraintSourceConfigFlagOrTarget
            ]

        . addConstraints
            -- '--enable-tests' and '--enable-benchmarks' constraints from
            -- the config file or command line
            [ let pc = PackageConstraintStanzas (packageName pkg) $
                       [ TestStanzas  | testsEnabled ] ++
                       [ BenchStanzas | benchmarksEnabled ]
              in LabeledPackageConstraint pc ConstraintSourceConfigFlagOrTarget
            ]

        $ standardInstallPolicy
            installedPkgIndex
            (SourcePackageDb mempty packagePrefs)
            [SpecificSourcePackage localPkg]

  return (resolveDependencies platform (compilerInfo comp) solver resolverParams)


-- | Call an installer for an 'SourcePackage' but override the configure
-- flags with the ones given by the 'ReadyPackage'. In particular the
-- 'ReadyPackage' specifies an exact 'FlagAssignment' and exactly
-- versioned package dependencies. So we ignore any previous partial flag
-- assignment or dependency constraints and use the new ones.
--
-- NB: when updating this function, don't forget to also update
-- 'installReadyPackage' in D.C.Install.
configurePackage :: Verbosity
                 -> Platform -> CompilerInfo
                 -> SetupScriptOptions
                 -> ConfigFlags
                 -> ReadyPackage
                 -> [String]
                 -> IO ()
configurePackage verbosity platform comp scriptOptions configFlags
                 (ReadyPackage (ConfiguredPackage (SourcePackage _ gpkg _ _)
                                                  flags stanzas _)
                               deps)
                 extraArgs =

  setupWrapper verbosity
    scriptOptions (Just pkg) configureCommand configureFlags extraArgs

  where
    configureFlags   = filterConfigureFlags configFlags {
      configConfigurationsFlags = flags,
      -- We generate the legacy constraints as well as the new style precise
      -- deps.  In the end only one set gets passed to Setup.hs configure,
      -- depending on the Cabal version we are talking to.
      configConstraints  = [ thisPackageVersion (packageId deppkg)
                           | deppkg <- CD.nonSetupDeps deps ],
      configDependencies = [ (packageName (Installed.sourcePackageId deppkg),
                              Installed.installedPackageId deppkg)
                           | deppkg <- CD.nonSetupDeps deps ],
      -- Use '--exact-configuration' if supported.
      configExactConfiguration = toFlag True,
      configVerbosity          = toFlag verbosity,
      configBenchmarks         = toFlag (BenchStanzas `elem` stanzas),
      configTests              = toFlag (TestStanzas `elem` stanzas)
    }

    pkg = case finalizePackageDescription flags
           (const True)
           platform comp [] (enableStanzas stanzas gpkg) of
      Left _ -> error "finalizePackageDescription ReadyPackage failed"
      Right (desc, _) -> desc
