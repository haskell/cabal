{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | cabal-install CLI command: build
module Distribution.Client.CmdInstall
  ( -- * The @build@ CLI and action
    installCommand
  , installAction

    -- * Internals exposed for testing
  , selectPackageTargets
  , selectComponentTarget

    -- * Internals exposed for CmdRepl + CmdRun
  , establishDummyDistDirLayout
  , establishDummyProjectBaseContext
  ) where

import Distribution.Client.Compat.Prelude
import Distribution.Compat.Directory
  ( doesPathExist
  )
import Prelude ()

import Distribution.Client.CmdErrorMessages
import Distribution.Client.CmdSdist
import Distribution.Client.ProjectOrchestration
import Distribution.Client.TargetProblem
  ( TargetProblem (..)
  , TargetProblem'
  )

import Distribution.Client.CmdInstall.ClientInstallFlags
import Distribution.Client.CmdInstall.ClientInstallTargetSelector

import Distribution.Client.Config
  ( SavedConfig (..)
  , defaultInstallPath
  , loadConfig
  )
import Distribution.Client.DistDirLayout
  ( CabalDirLayout (..)
  , DistDirLayout (..)
  , StoreDirLayout (..)
  , cabalStoreDirLayout
  , mkCabalDirLayout
  )
import Distribution.Client.IndexUtils
  ( getInstalledPackages
  , getSourcePackages
  )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallSymlink
  ( Symlink (..)
  , promptRun
  , symlinkBinary
  , symlinkableBinary
  , trySymlink
  )
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , cfgVerbosity
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectConfig
  ( ProjectPackageLocation (..)
  , fetchAndReadSourcePackages
  , projectConfigWithBuilderRepoContext
  , resolveBuildTimeSettings
  , withGlobalConfig
  , withProjectOrGlobalConfig
  )
import Distribution.Client.ProjectConfig.Types
  ( MapMappend (..)
  , PackageConfig (..)
  , ProjectConfig (..)
  , ProjectConfigBuildOnly (..)
  , ProjectConfigShared (..)
  , getMapLast
  , getMapMappend
  , projectConfigBuildOnly
  , projectConfigConfigFile
  , projectConfigLogsDir
  , projectConfigStoreDir
  )
import Distribution.Client.ProjectFlags (ProjectFlags (..))
import Distribution.Client.ProjectPlanning
  ( storePackageInstallDirs'
  )
import Distribution.Client.ProjectPlanning.Types
  ( ElaboratedInstallPlan
  )
import Distribution.Client.RebuildMonad
  ( runRebuild
  )
import Distribution.Client.Setup
  ( ConfigFlags (..)
  , GlobalFlags (..)
  , InstallFlags (..)
  )
import Distribution.Client.Types
  ( PackageLocation (..)
  , PackageSpecifier (..)
  , SourcePackageDb (..)
  , UnresolvedSourcePackage
  , mkNamedPackage
  , pkgSpecifierTarget
  )
import Distribution.Client.Types.OverwritePolicy
  ( OverwritePolicy (..)
  )
import Distribution.Package
  ( Package (..)
  , PackageName
  , mkPackageName
  , unPackageName
  )
import Distribution.Simple.BuildPaths
  ( exeExtension
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , optionName
  , usageAlternatives
  )
import Distribution.Simple.Compiler
  ( Compiler (..)
  , CompilerFlavor (..)
  , CompilerId (..)
  , PackageDBCWD
  , PackageDBStackCWD
  , PackageDBX (..)
  )
import Distribution.Simple.Configure
  ( configCompilerEx
  , interpretPackageDbFlags
  )
import Distribution.Simple.Flag
  ( flagElim
  , flagToMaybe
  , fromFlagOrDefault
  )
import Distribution.Simple.GHC
  ( GhcEnvironmentFileEntry (..)
  , GhcImplInfo (..)
  , ParseErrorExc
  , getGhcAppDir
  , getImplInfo
  , ghcPlatformAndVersionString
  , readGhcEnvironmentFile
  , renderGhcEnvironmentFile
  )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import qualified Distribution.Simple.PackageIndex as PI
import Distribution.Simple.Program.Db
  ( defaultProgramDb
  , prependProgramSearchPath
  , userSpecifyArgss
  , userSpecifyPaths
  )
import Distribution.Simple.Setup
  ( Flag
  , installDirsOptions
  , pattern Flag
  , pattern NoFlag
  )
import Distribution.Simple.Utils
  ( createDirectoryIfMissingVerbose
  , dieWithException
  , notice
  , ordNub
  , safeHead
  , warn
  , withTempDirectory
  , wrapText
  )
import Distribution.Solver.Types.PackageConstraint
  ( PackageProperty (..)
  )
import Distribution.Solver.Types.PackageIndex
  ( lookupPackageName
  , searchByName
  )
import Distribution.Solver.Types.SourcePackage
  ( SourcePackage (..)
  )
import Distribution.System
  ( OS (Windows)
  , Platform
  , buildOS
  )
import Distribution.Types.InstalledPackageInfo
  ( InstalledPackageInfo (..)
  )
import Distribution.Types.PackageId
  ( PackageIdentifier (..)
  )
import Distribution.Types.UnitId
  ( UnitId
  )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName
  , unUnqualComponentName
  )
import Distribution.Types.Version
  ( Version
  , nullVersion
  )
import Distribution.Types.VersionRange
  ( thisVersion
  )
import Distribution.Utils.Generic
  ( writeFileAtomic
  )
import Distribution.Verbosity
  ( lessVerbose
  , normal
  )

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Ord
  ( Down (..)
  )
import qualified Data.Set as S
import Distribution.Client.Errors
import Distribution.Utils.NubList
  ( fromNubList
  )
import Network.URI (URI)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getTemporaryDirectory
  , makeAbsolute
  , removeDirectory
  , removeFile
  )
import System.FilePath
  ( takeBaseName
  , takeDirectory
  , (<.>)
  , (</>)
  )

-- | Check or check then install an exe. The check is to see if the overwrite
-- policy allows installation.
data InstallCheck
  = -- | Only check if install is permitted.
    InstallCheckOnly
  | -- | Actually install but check first if permitted.
    InstallCheckInstall

type InstallAction =
  Verbosity
  -> OverwritePolicy
  -> InstallExe
  -> (UnitId, [(ComponentTarget, NonEmpty TargetSelector)])
  -> IO ()

data InstallCfg = InstallCfg
  { verbosity :: Verbosity
  , baseCtx :: ProjectBaseContext
  , buildCtx :: ProjectBuildContext
  , platform :: Platform
  , compiler :: Compiler
  , installConfigFlags :: ConfigFlags
  , installClientFlags :: ClientInstallFlags
  }

-- | A record of install method, install directory and file path functions
-- needed by actions that either check if an install is possible or actually
-- perform an installation. This is for installation of executables only.
data InstallExe = InstallExe
  { installMethod :: InstallMethod
  , installDir :: FilePath
  , mkSourceBinDir :: UnitId -> FilePath
  -- ^ A function to get an UnitId's store directory.
  , mkExeName :: UnqualComponentName -> FilePath
  -- ^ A function to get an exe's filename.
  , mkFinalExeName :: UnqualComponentName -> FilePath
  -- ^ A function to get an exe's final possibly different to the name in the
  -- store.
  }

installCommand :: CommandUI (NixStyleFlags ClientInstallFlags)
installCommand =
  CommandUI
    { commandName = "v2-install"
    , commandSynopsis = "Install packages."
    , commandUsage =
        usageAlternatives
          "v2-install"
          ["[TARGETS] [FLAGS]"]
    , commandDescription = Just $ \_ ->
        wrapText $
          "Installs one or more packages. This is done by installing them "
            ++ "in the store and symlinking or copying the executables in the directory "
            ++ "specified by the --installdir flag (`~/.local/bin/` by default). "
            ++ "If you want the installed executables to be available globally, "
            ++ "make sure that the PATH environment variable contains that directory. "
            ++ "\n\n"
            ++ "If TARGET is a library and --lib (provisional) is used, "
            ++ "it will be added to the global environment. "
            ++ "When doing this, cabal will try to build a plan that includes all "
            ++ "the previously installed libraries. This is currently not implemented."
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v2-install\n"
          ++ "    Install the package in the current directory\n"
          ++ "  "
          ++ pname
          ++ " v2-install pkgname\n"
          ++ "    Install the package named pkgname"
          ++ " (fetching it from hackage if necessary)\n"
          ++ "  "
          ++ pname
          ++ " v2-install ./pkgfoo\n"
          ++ "    Install the package in the ./pkgfoo directory\n"
    , commandOptions = \x -> filter notInstallDirOpt $ nixStyleOptions clientInstallOptions x
    , commandDefaultFlags = defaultNixStyleFlags defaultClientInstallFlags
    }
  where
    -- install doesn't take installDirs flags, since it always installs into the store in a fixed way.
    notInstallDirOpt x = notElem (optionName x) installDirOptNames
    installDirOptNames = map optionName installDirsOptions

-- | The @install@ command actually serves four different needs. It installs:
-- * exes:
--   For example a program from hackage. The behavior is similar to the old
--   install command, except that now conflicts between separate runs of the
--   command are impossible thanks to the store.
--   Exes are installed in the store like a normal dependency, then they are
--   symlinked/copied in the directory specified by --installdir.
--   To do this we need a dummy projectBaseContext containing the targets as
--   extra packages and using a temporary dist directory.
-- * libraries
--   Libraries install through a similar process, but using GHC environment
--   files instead of symlinks. This means that 'v2-install'ing libraries
--   only works on GHC >= 8.0.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
installAction :: NixStyleFlags ClientInstallFlags -> [String] -> GlobalFlags -> IO ()
installAction flags@NixStyleFlags{extraFlags, configFlags, installFlags, projectFlags} targetStrings globalFlags = do
  -- Ensure there were no invalid configuration options specified.
  verifyPreconditionsOrDie verbosity configFlags'

  -- We cannot use establishDummyProjectBaseContext to get these flags, since
  -- it requires one of them as an argument. Normal establishProjectBaseContext
  -- does not, and this is why this is done only for the install command
  clientInstallFlags <- getClientInstallFlags verbosity globalFlags extraFlags
  let
    installLibs = fromFlagOrDefault False (cinstInstallLibs clientInstallFlags)

    normalisedTargetStrings = if null targetStrings then ["."] else targetStrings

  -- Note the logic here is rather goofy. Target selectors of the form "foo:bar" also parse as uris.
  -- However, we want install to also take uri arguments. Hence, we only parse uri arguments in the case where
  -- no project file is present (including an implicit one derived from being in a package directory)
  -- or where the --ignore-project flag is passed explicitly. In such a case we only parse colon-free target selectors
  -- as selectors, and otherwise parse things as URIs.

  -- However, in the special case where --ignore-project is passed with no selectors, we want to act as though this is
  -- a "normal" ignore project that actually builds and installs the selected package.

  (pkgSpecs, uris, targetSelectors, config) <-
    let
      with = do
        (pkgSpecs, targetSelectors, baseConfig) <-
          withProject verbosity cliConfig normalisedTargetStrings installLibs
        -- No URIs in this case, see note above
        return (pkgSpecs, [], targetSelectors, baseConfig)

      without =
        withGlobalConfig verbosity globalConfigFlag $ \globalConfig ->
          withoutProject verbosity (globalConfig <> cliConfig) normalisedTargetStrings
     in
      -- If there's no targets it does not make sense to not be in a project.
      if null targetStrings
        then with
        else withProjectOrGlobalConfig ignoreProject with without

  -- NOTE: CmdInstall and project local packages.
  --
  -- CmdInstall always installs packages from a source distribution that, in case of unpackage
  -- packages, is created automatically. This is implemented in getSpecsAndTargetSelectors.
  --
  -- This has the inconvenience that the planner will consider all packages as non-local
  -- (see `ProjectPlanning.shouldBeLocal`) and that any project or cli configuration will
  -- not apply to them.
  --
  -- We rectify this here. In the project configuration, we copy projectConfigLocalPackages to a
  -- new projectConfigSpecificPackage entry for each package corresponding to a target selector.
  --
  -- See #8637 and later #7297, #8909, #7236.

  let
    ProjectConfig
      { projectConfigBuildOnly =
        ProjectConfigBuildOnly
          { projectConfigLogsDir
          }
      , projectConfigShared =
        ProjectConfigShared
          { projectConfigHcFlavor
          , projectConfigHcPath
          , projectConfigHcPkg
          , projectConfigStoreDir
          , projectConfigProgPathExtra
          , projectConfigPackageDBs
          }
      , projectConfigLocalPackages =
        PackageConfig
          { packageConfigProgramPaths
          , packageConfigProgramArgs
          , packageConfigProgramPathExtra
          }
      } = config

    hcFlavor = flagToMaybe projectConfigHcFlavor
    hcPath = flagToMaybe projectConfigHcPath
    hcPkg = flagToMaybe projectConfigHcPkg
    extraPath = fromNubList packageConfigProgramPathExtra ++ fromNubList projectConfigProgPathExtra

  configProgDb <- prependProgramSearchPath verbosity extraPath [] defaultProgramDb
  let
    -- ProgramDb with directly user specified paths
    preProgDb =
      userSpecifyPaths (Map.toList (getMapLast packageConfigProgramPaths))
        . userSpecifyArgss (Map.toList (getMapMappend packageConfigProgramArgs))
        $ configProgDb

  -- progDb is a program database with compiler tools configured properly
  (compiler@Compiler{compilerId = CompilerId compilerFlavor compilerVersion}, platform, progDb) <-
    configCompilerEx hcFlavor hcPath hcPkg preProgDb verbosity

  let
    GhcImplInfo{supportsPkgEnvFiles} = getImplInfo compiler

  (usedPackageEnvFlag, envFile) <- getEnvFile clientInstallFlags platform compilerVersion
  (usedExistingPkgEnvFile, existingEnvEntries) <-
    getExistingEnvEntries verbosity compilerFlavor supportsPkgEnvFiles envFile
  packageDbs <- getPackageDbStack compiler projectConfigStoreDir projectConfigLogsDir projectConfigPackageDBs
  installedIndex <- getInstalledPackages verbosity compiler packageDbs progDb

  let
    (envSpecs, nonGlobalEnvEntries) =
      getEnvSpecsAndNonGlobalEntries installedIndex existingEnvEntries installLibs

  -- Second, we need to use a fake project to let Cabal build the
  -- installables correctly. For that, we need a place to put a
  -- temporary dist directory.
  globalTmp <- getTemporaryDirectory

  withTempDirectory verbosity globalTmp "cabal-install." $ \tmpDir -> do
    distDirLayout <- establishDummyDistDirLayout verbosity config tmpDir

    uriSpecs <-
      runRebuild tmpDir $
        fetchAndReadSourcePackages
          verbosity
          distDirLayout
          (Just compiler)
          (projectConfigShared config)
          (projectConfigBuildOnly config)
          [ProjectPackageRemoteTarball uri | uri <- uris]

    -- check for targets already in env
    let getPackageName :: PackageSpecifier UnresolvedSourcePackage -> PackageName
        getPackageName = pkgSpecifierTarget
        targetNames = S.fromList $ map getPackageName (pkgSpecs ++ uriSpecs)
        envNames = S.fromList $ map getPackageName envSpecs
        forceInstall = fromFlagOrDefault False $ installOverrideReinstall installFlags
        nameIntersection = S.intersection targetNames envNames

    -- we check for intersections in targets with the existing env
    (envSpecs', nonGlobalEnvEntries') <-
      if null nameIntersection
        then pure (envSpecs, map snd nonGlobalEnvEntries)
        else
          if forceInstall
            then
              let es = filter (\e -> not $ getPackageName e `S.member` nameIntersection) envSpecs
                  nge = map snd . filter (\e -> not $ fst e `S.member` nameIntersection) $ nonGlobalEnvEntries
               in pure (es, nge)
            else dieWithException verbosity $ PackagesAlreadyExistInEnvfile envFile (map prettyShow $ S.toList nameIntersection)

    -- we construct an installed index of files in the cleaned target environment (absent overwrites) so that
    -- we can solve with regards to packages installed locally but not in the upstream repo
    let installedPacks = PI.allPackagesByName installedIndex
        newEnvNames = S.fromList $ map getPackageName envSpecs'
        installedIndex' = PI.fromList . concatMap snd . filter (\p -> fst p `S.member` newEnvNames) $ installedPacks

    baseCtx <-
      establishDummyProjectBaseContext
        verbosity
        config
        distDirLayout
        (envSpecs' ++ pkgSpecs ++ uriSpecs)
        InstallCommand

    buildCtx <- constructProjectBuildContext verbosity (baseCtx{installedPackages = Just installedIndex'}) targetSelectors

    printPlan verbosity baseCtx buildCtx
    let installCfg = InstallCfg verbosity baseCtx buildCtx platform compiler configFlags clientInstallFlags

    let
      dryRun =
        buildSettingDryRun (buildSettings baseCtx)
          || buildSettingOnlyDownload (buildSettings baseCtx)

    -- Before building, check if we could install any built exe by symlinking or
    -- copying it?
    unless
      (dryRun || installLibs)
      (traverseInstall (installCheckUnitExes InstallCheckOnly) installCfg)

    buildOutcomes <- runProjectBuildPhase verbosity baseCtx buildCtx
    runProjectPostBuildPhase verbosity baseCtx buildCtx buildOutcomes

    -- Having built everything, do the install.
    unless dryRun $
      if installLibs
        then
          installLibraries
            verbosity
            buildCtx
            installedIndex
            compiler
            packageDbs
            envFile
            nonGlobalEnvEntries'
            (not usedExistingPkgEnvFile && not usedPackageEnvFlag)
        else -- Install any built exe by symlinking or copying it we don't use
        -- BuildOutcomes because we also need the component names
          traverseInstall (installCheckUnitExes InstallCheckInstall) installCfg
  where
    configFlags' = disableTestsBenchsByDefault . ignoreProgramAffixes $ configFlags
    verbosity = cfgVerbosity normal flags
    ignoreProject = flagIgnoreProject projectFlags
    cliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags{configFlags = configFlags'}
        extraFlags

    globalConfigFlag = projectConfigConfigFile (projectConfigShared cliConfig)

    -- Do the install action for each executable in the install configuration.
    traverseInstall :: InstallAction -> InstallCfg -> IO ()
    traverseInstall action cfg@InstallCfg{verbosity = v, buildCtx, installClientFlags} = do
      let overwritePolicy = fromFlagOrDefault NeverOverwrite $ cinstOverwritePolicy installClientFlags
      actionOnExe <- action v overwritePolicy <$> prepareExeInstall cfg
      traverse_ actionOnExe . Map.toList $ targetsMap buildCtx

withProject
  :: Verbosity
  -> ProjectConfig
  -> [String]
  -> Bool
  -> IO ([PackageSpecifier UnresolvedSourcePackage], [TargetSelector], ProjectConfig)
withProject verbosity cliConfig targetStrings installLibs = do
  -- First, we need to learn about what's available to be installed.
  baseCtx <- establishProjectBaseContext reducedVerbosity cliConfig InstallCommand

  (pkgSpecs, targetSelectors) <-
    -- If every target is already resolved to a package id, we can return without any further parsing.
    if null unresolvedTargetStrings
      then return (parsedPkgSpecs, parsedTargets)
      else do
        -- Anything that could not be parsed as a packageId (e.g. a package name without a version or
        -- a target syntax using colons) must be resolved inside the project context.
        (resolvedPkgSpecs, resolvedTargets) <-
          resolveTargetSelectorsInProjectBaseContext verbosity baseCtx unresolvedTargetStrings targetFilter
        return (resolvedPkgSpecs ++ parsedPkgSpecs, resolvedTargets ++ parsedTargets)

  -- Apply the local configuration (e.g. cli flags) to all direct targets of install command, see note
  -- in 'installAction'.
  --
  -- NOTE: If a target string had to be resolved inside the project context, then pkgSpecs will include
  -- the project packages turned into source distributions (getSpecsAndTargetSelectors does this).
  -- We want to apply the local configuration only to the actual targets.
  let config =
        addLocalConfigToPkgs (projectConfig baseCtx) $
          concatMap (targetPkgNames $ localPackages baseCtx) targetSelectors
  return (pkgSpecs, targetSelectors, config)
  where
    reducedVerbosity = lessVerbose verbosity

    -- We take the targets and try to parse them as package ids (with name and version).
    -- The ones who don't parse will have to be resolved in the project context.
    (unresolvedTargetStrings, parsedPackageIds) =
      partitionEithers $
        flip map targetStrings $ \s ->
          case eitherParsec s of
            Right pkgId@PackageIdentifier{pkgVersion}
              | pkgVersion /= nullVersion ->
                  pure pkgId
            _ -> Left s

    -- For each packageId, we output a NamedPackage specifier (i.e. a package only known by
    -- its name) and a target selector.
    (parsedPkgSpecs, parsedTargets) =
      unzip
        [ (mkNamedPackage pkgId, TargetPackageNamed (pkgName pkgId) targetFilter)
        | pkgId <- parsedPackageIds
        ]

    targetFilter = if installLibs then Just LibKind else Just ExeKind

resolveTargetSelectorsInProjectBaseContext
  :: Verbosity
  -> ProjectBaseContext
  -> [String]
  -> Maybe ComponentKindFilter
  -> IO ([PackageSpecifier UnresolvedSourcePackage], [TargetSelector])
resolveTargetSelectorsInProjectBaseContext verbosity baseCtx targetStrings targetFilter = do
  let reducedVerbosity = lessVerbose verbosity

  sourcePkgDb <-
    projectConfigWithBuilderRepoContext
      reducedVerbosity
      (buildSettings baseCtx)
      (getSourcePackages verbosity)

  targetSelectors <-
    readTargetSelectors (localPackages baseCtx) Nothing targetStrings
      >>= \case
        Left problems -> reportTargetSelectorProblems verbosity problems
        Right ts -> return ts

  getSpecsAndTargetSelectors
    verbosity
    reducedVerbosity
    sourcePkgDb
    targetSelectors
    (distDirLayout baseCtx)
    baseCtx
    targetFilter

withoutProject
  :: Verbosity
  -> ProjectConfig
  -> [String]
  -> IO ([PackageSpecifier UnresolvedSourcePackage], [URI], [TargetSelector], ProjectConfig)
withoutProject verbosity globalConfig targetStrings = do
  tss <- traverse (parseWithoutProjectTargetSelector verbosity) targetStrings
  let
    ProjectConfigBuildOnly
      { projectConfigLogsDir
      } = projectConfigBuildOnly globalConfig

    ProjectConfigShared
      { projectConfigStoreDir
      } = projectConfigShared globalConfig

    mlogsDir = flagToMaybe projectConfigLogsDir
    mstoreDir = flagToMaybe projectConfigStoreDir

  cabalDirLayout <- mkCabalDirLayout mstoreDir mlogsDir

  let buildSettings = resolveBuildTimeSettings verbosity cabalDirLayout globalConfig

  SourcePackageDb{packageIndex} <-
    projectConfigWithBuilderRepoContext
      verbosity
      buildSettings
      (getSourcePackages verbosity)

  for_ (concatMap woPackageNames tss) $ \name -> do
    when (null (lookupPackageName packageIndex name)) $ do
      let xs = searchByName packageIndex (unPackageName name)
      let emptyIf True _ = []
          emptyIf False zs = zs
          str2 =
            emptyIf
              (null xs)
              [ "Did you mean any of the following?\n"
              , unlines (("- " ++) . unPackageName . fst <$> xs)
              ]
      dieWithException verbosity $ WithoutProject (unPackageName name) str2

  let
    packageSpecifiers :: [PackageSpecifier UnresolvedSourcePackage]
    (uris, packageSpecifiers) = partitionEithers $ map woPackageSpecifiers tss
    packageTargets = map woPackageTargets tss

  -- Apply the local configuration (e.g. cli flags) to all direct targets of install command,
  -- see note in 'installAction'
  let config = addLocalConfigToPkgs globalConfig (concatMap woPackageNames tss)
  return (packageSpecifiers, uris, packageTargets, config)

addLocalConfigToPkgs :: ProjectConfig -> [PackageName] -> ProjectConfig
addLocalConfigToPkgs config pkgs =
  config
    { projectConfigSpecificPackage =
        projectConfigSpecificPackage config
          <> MapMappend (Map.fromList targetPackageConfigs)
    }
  where
    localConfig = projectConfigLocalPackages config
    targetPackageConfigs = map (,localConfig) pkgs

targetPkgNames
  :: [PackageSpecifier UnresolvedSourcePackage]
  -- ^ The local packages, to resolve 'TargetAllPackages' selectors
  -> TargetSelector
  -> [PackageName]
targetPkgNames localPkgs = \case
  TargetPackage _ pkgIds _ -> map pkgName pkgIds
  TargetPackageNamed name _ -> [name]
  TargetAllPackages _ -> map pkgSpecifierTarget localPkgs
  -- Note how the target may select a component only, but we will always apply
  -- the local flags to the whole package in which that component is contained.
  -- The reason is that our finest level of configuration is per-package, so
  -- there is no interface to configure options to a component only. It is not
  -- trivial to say whether we could indeed support per-component configuration
  -- because of legacy packages which we may always have to build whole.
  TargetComponent pkgId _ _ -> [pkgName pkgId]
  TargetComponentUnknown name _ _ -> [name]

-- | Verify that invalid config options were not passed to the install command.
--
-- If an invalid configuration is found the command will @dieWithException@.
verifyPreconditionsOrDie :: Verbosity -> ConfigFlags -> IO ()
verifyPreconditionsOrDie verbosity configFlags = do
  -- We never try to build tests/benchmarks for remote packages.
  -- So we set them as disabled by default and error if they are explicitly
  -- enabled.
  when (configTests configFlags == Flag True) $
    dieWithException verbosity ConfigTests
  when (configBenchmarks configFlags == Flag True) $
    dieWithException verbosity ConfigBenchmarks

-- | Apply the given 'ClientInstallFlags' on top of one coming from the global configuration.
getClientInstallFlags :: Verbosity -> GlobalFlags -> ClientInstallFlags -> IO ClientInstallFlags
getClientInstallFlags verbosity globalFlags existingClientInstallFlags = do
  let configFileFlag = globalConfigFile globalFlags
  savedConfig <- loadConfig verbosity configFileFlag
  pure $ savedClientInstallFlags savedConfig `mappend` existingClientInstallFlags

getSpecsAndTargetSelectors
  :: Verbosity
  -> Verbosity
  -> SourcePackageDb
  -> [TargetSelector]
  -> DistDirLayout
  -> ProjectBaseContext
  -> Maybe ComponentKindFilter
  -> IO ([PackageSpecifier UnresolvedSourcePackage], [TargetSelector])
getSpecsAndTargetSelectors verbosity reducedVerbosity sourcePkgDb targetSelectors distDirLayout baseCtx targetFilter =
  withInstallPlan reducedVerbosity baseCtx $ \elaboratedPlan _ -> do
    -- Split into known targets and hackage packages.
    (targetsMap, hackageNames) <-
      partitionToKnownTargetsAndHackagePackages
        verbosity
        sourcePkgDb
        elaboratedPlan
        targetSelectors

    let
      planMap = InstallPlan.toMap elaboratedPlan

      sdistize (SpecificSourcePackage spkg) =
        SpecificSourcePackage spkg'
        where
          sdistPath = distSdistFile distDirLayout (packageId spkg)
          spkg' = spkg{srcpkgSource = LocalTarballPackage sdistPath}
      sdistize named = named

      localPkgs = sdistize <$> localPackages baseCtx

      gatherTargets :: UnitId -> TargetSelector
      gatherTargets targetId = TargetPackageNamed pkgName targetFilter
        where
          targetUnit = Map.findWithDefault (error "cannot find target unit") targetId planMap
          PackageIdentifier{..} = packageId targetUnit

      localTargets = map gatherTargets (Map.keys targetsMap)

      hackagePkgs :: [PackageSpecifier UnresolvedSourcePackage]
      hackagePkgs = [NamedPackage pn [] | pn <- hackageNames]

      hackageTargets :: [TargetSelector]
      hackageTargets = [TargetPackageNamed pn targetFilter | pn <- hackageNames]

    createDirectoryIfMissing True (distSdistDirectory distDirLayout)

    unless (Map.null targetsMap) $ for_ (localPackages baseCtx) $ \case
      SpecificSourcePackage pkg ->
        packageToSdist
          verbosity
          (distProjectRootDirectory distDirLayout)
          TarGzArchive
          (distSdistFile distDirLayout (packageId pkg))
          pkg
      NamedPackage _ _ ->
        -- This may happen if 'extra-packages' are listed in the project file.
        -- We don't need to do extra work for NamedPackages since they will be
        -- fetched from Hackage rather than locally 'sdistize'-d. Note how,
        -- below, we already return the local 'sdistize'-d packages together
        -- with the 'hackagePkgs' (which are 'NamedPackage's), and that
        -- 'sdistize' is a no-op for 'NamedPackages', meaning the
        -- 'NamedPackage's in 'localPkgs' will be treated just like
        -- 'hackagePkgs' as they should.
        pure ()

    if null targetsMap
      then return (hackagePkgs, hackageTargets)
      else return (localPkgs ++ hackagePkgs, localTargets ++ hackageTargets)

-- | Partitions the target selectors into known local targets and hackage packages.
partitionToKnownTargetsAndHackagePackages
  :: Verbosity
  -> SourcePackageDb
  -> ElaboratedInstallPlan
  -> [TargetSelector]
  -> IO (TargetsMap, [PackageName])
partitionToKnownTargetsAndHackagePackages verbosity pkgDb elaboratedPlan targetSelectors = do
  let mTargets =
        resolveTargetsFromSolver
          selectPackageTargets
          selectComponentTarget
          elaboratedPlan
          (Just pkgDb)
          targetSelectors
  case mTargets of
    Right targets ->
      -- Everything is a local dependency.
      return (targets, [])
    Left errs -> do
      -- Not everything is local.
      let
        (errs', hackageNames) = partitionEithers . flip fmap errs $ \case
          TargetAvailableInIndex name -> Right name
          err -> Left err

      -- report incorrect case for known package.
      for_ errs' $ \case
        TargetNotInProject hn ->
          case searchByName (packageIndex pkgDb) (unPackageName hn) of
            [] -> return ()
            xs ->
              dieWithException verbosity $ UnknownPackage (unPackageName hn) (("- " ++) . unPackageName . fst <$> xs)
        _ -> return ()

      when (not . null $ errs') $ reportBuildTargetProblems verbosity errs'

      let
        targetSelectors' = flip filter targetSelectors $ \case
          TargetComponentUnknown name _ _
            | name `elem` hackageNames -> False
          TargetPackageNamed name _
            | name `elem` hackageNames -> False
          _ -> True

      -- This can't fail, because all of the errors are
      -- removed (or we've given up).
      targets <-
        either (reportBuildTargetProblems verbosity) return $
          resolveTargetsFromSolver
            selectPackageTargets
            selectComponentTarget
            elaboratedPlan
            Nothing
            targetSelectors'

      return (targets, hackageNames)

constructProjectBuildContext
  :: Verbosity
  -> ProjectBaseContext
  -- ^ The synthetic base context to use to produce the full build context.
  -> [TargetSelector]
  -> IO ProjectBuildContext
constructProjectBuildContext verbosity baseCtx targetSelectors = do
  runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan -> do
    -- Interpret the targets on the command line as build targets
    targets <-
      either (reportBuildTargetProblems verbosity) return $
        resolveTargetsFromSolver
          selectPackageTargets
          selectComponentTarget
          elaboratedPlan
          Nothing
          targetSelectors

    let prunedToTargetsElaboratedPlan =
          pruneInstallPlanToTargets TargetActionBuild targets elaboratedPlan
    prunedElaboratedPlan <-
      if buildSettingOnlyDeps (buildSettings baseCtx)
        then
          either (reportCannotPruneDependencies verbosity) return $
            pruneInstallPlanToDependencies
              (Map.keysSet targets)
              prunedToTargetsElaboratedPlan
        else return prunedToTargetsElaboratedPlan

    return (prunedElaboratedPlan, targets)

-- | From an install configuration, prepare the record needed by actions that
-- will either check if an install of a single executable is possible or
-- actually perform its installation.
prepareExeInstall :: InstallCfg -> IO InstallExe
prepareExeInstall
  InstallCfg{verbosity, baseCtx, buildCtx, platform, compiler, installConfigFlags, installClientFlags} = do
    installPath <- defaultInstallPath
    let storeDirLayout = cabalStoreDirLayout $ cabalDirLayout baseCtx

        prefix = fromFlagOrDefault "" (fmap InstallDirs.fromPathTemplate (configProgPrefix installConfigFlags))
        suffix = fromFlagOrDefault "" (fmap InstallDirs.fromPathTemplate (configProgSuffix installConfigFlags))

        mkUnitBinDir :: UnitId -> FilePath
        mkUnitBinDir =
          InstallDirs.bindir
            . storePackageInstallDirs' storeDirLayout compiler

        mkExeName :: UnqualComponentName -> FilePath
        mkExeName exe = unUnqualComponentName exe <.> exeExtension platform

        mkFinalExeName :: UnqualComponentName -> FilePath
        mkFinalExeName exe = prefix <> unUnqualComponentName exe <> suffix <.> exeExtension platform
        installdirUnknown =
          "installdir is not defined. Set it in your cabal config file "
            ++ "or use --installdir=<path>. Using default installdir: "
            ++ show installPath

    installdir <-
      fromFlagOrDefault
        (warn verbosity installdirUnknown >> pure installPath)
        $ pure <$> cinstInstalldir installClientFlags
    createDirectoryIfMissingVerbose verbosity True installdir
    warnIfNoExes verbosity buildCtx

    -- This is in IO as we will make environment checks, to decide which install
    -- method is best.
    let defaultMethod :: IO InstallMethod
        defaultMethod
          -- Try symlinking in temporary directory, if it works default to
          -- symlinking even on windows.
          | buildOS == Windows = do
              symlinks <- trySymlink verbosity
              return $ if symlinks then InstallMethodSymlink else InstallMethodCopy
          | otherwise = return InstallMethodSymlink

    installMethod <- flagElim defaultMethod return $ cinstInstallMethod installClientFlags

    return $ InstallExe installMethod installdir mkUnitBinDir mkExeName mkFinalExeName

-- | Install any built library by adding it to the default ghc environment
installLibraries
  :: Verbosity
  -> ProjectBuildContext
  -> PI.PackageIndex InstalledPackageInfo
  -> Compiler
  -> PackageDBStackCWD
  -> FilePath
  -- ^ Environment file
  -> [GhcEnvironmentFileEntry FilePath]
  -> Bool
  -- ^ Whether we need to show a warning (i.e. we created a new environment
  --   file, and the user did not use --package-env)
  -> IO ()
installLibraries
  verbosity
  buildCtx
  installedIndex
  compiler
  packageDbs'
  envFile
  envEntries
  showWarning = do
    if supportsPkgEnvFiles $ getImplInfo compiler
      then do
        let validDb (SpecificPackageDB fp) = doesPathExist fp
            validDb _ = pure True
        -- if a user "installs" a global package and no existing cabal db exists, none will be created.
        -- this ensures we don't add the "phantom" path to the file.
        packageDbs <- filterM validDb packageDbs'
        let
          getLatest =
            (=<<) (maybeToList . safeHead . snd)
              . take 1
              . sortBy (comparing (Down . fst))
              . PI.lookupPackageName installedIndex
          globalLatest = concat (getLatest <$> globalPackages)
          globalEntries = GhcEnvFilePackageId . installedUnitId <$> globalLatest
          baseEntries =
            GhcEnvFileClearPackageDbStack : fmap GhcEnvFilePackageDb packageDbs
          pkgEntries =
            ordNub $
              globalEntries
                ++ envEntries
                ++ entriesForLibraryComponents (targetsMap buildCtx)
          contents' = renderGhcEnvironmentFile (baseEntries ++ pkgEntries)
        createDirectoryIfMissing True (takeDirectory envFile)
        writeFileAtomic envFile (BS.pack contents')
        when showWarning $
          warn verbosity $
            "The libraries were installed by creating a global GHC environment file at:\n"
              ++ envFile
              ++ "\n"
              ++ "\n"
              ++ "The presence of such an environment file is likely to confuse or break other "
              ++ "tools because it changes GHC's behaviour: it changes the default package set in "
              ++ "ghc and ghci from its normal value (which is \"all boot libraries\"). GHC "
              ++ "environment files are little-used and often not tested for.\n"
              ++ "\n"
              ++ "Furthermore, management of these environment files is still more difficult than "
              ++ "it could be; see e.g. https://github.com/haskell/cabal/issues/6481 .\n"
              ++ "\n"
              ++ "Double-check that creating a global GHC environment file is really what you "
              ++ "wanted! You can limit the effects of the environment file by creating it in a "
              ++ "specific directory using the --package-env flag. For example, use:\n"
              ++ "\n"
              ++ "cabal install --lib <packages...> --package-env .\n"
              ++ "\n"
              ++ "to create the file in the current directory."
      else
        warn verbosity $
          "The current compiler doesn't support safely installing libraries, "
            ++ "so only executables will be available. (Library installation is "
            ++ "supported on GHC 8.0+ only)"

-- See ticket #8894. This is safe to include any nonreinstallable boot pkg,
-- but the particular package users will always expect to be in scope without specific installation
-- is base, so that they can access prelude, regardless of if they specifically asked for it.
globalPackages :: [PackageName]
globalPackages = mkPackageName <$> ["base"]

warnIfNoExes :: Verbosity -> ProjectBuildContext -> IO ()
warnIfNoExes verbosity buildCtx =
  when noExes $
    warn verbosity $
      "\n"
        <> "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
        <> "@ WARNING: Installation might not be completed as desired! @\n"
        <> "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
        <> "The command \"cabal install [TARGETS]\" doesn't expose libraries.\n"
        <> "* You might have wanted to add them as dependencies to your package."
        <> " In this case add \""
        <> intercalate ", " (showTargetSelector <$> selectors)
        <> "\" to the build-depends field(s) of your package's .cabal file.\n"
        <> "* You might have wanted to add them to a GHC environment. In this case"
        <> " use \"cabal install --lib "
        <> unwords (showTargetSelector <$> selectors)
        <> "\". "
        <> " The \"--lib\" flag is provisional: see"
        <> " https://github.com/haskell/cabal/issues/6481 for more information."
  where
    targets = concat $ Map.elems $ targetsMap buildCtx
    components = fst <$> targets
    selectors = concatMap (NE.toList . snd) targets
    noExes = null $ catMaybes $ exeMaybe <$> components

    exeMaybe (ComponentTarget (CExeName exe) _) = Just exe
    exeMaybe _ = Nothing

-- | Return the package specifiers and non-global environment file entries.
getEnvSpecsAndNonGlobalEntries
  :: PI.InstalledPackageIndex
  -> [GhcEnvironmentFileEntry FilePath]
  -> Bool
  -> ([PackageSpecifier a], [(PackageName, GhcEnvironmentFileEntry FilePath)])
getEnvSpecsAndNonGlobalEntries installedIndex entries installLibs =
  if installLibs
    then (envSpecs, envEntries')
    else ([], envEntries')
  where
    (envSpecs, envEntries') = environmentFileToSpecifiers installedIndex entries

environmentFileToSpecifiers
  :: PI.InstalledPackageIndex
  -> [GhcEnvironmentFileEntry FilePath]
  -> ([PackageSpecifier a], [(PackageName, GhcEnvironmentFileEntry FilePath)])
environmentFileToSpecifiers ipi = foldMap $ \case
  (GhcEnvFilePackageId unitId)
    | Just
        InstalledPackageInfo
          { sourcePackageId = PackageIdentifier{..}
          , installedUnitId
          } <-
        PI.lookupUnitId ipi unitId
    , let pkgSpec =
            NamedPackage
              pkgName
              [PackagePropertyVersion (thisVersion pkgVersion)] ->
        ([pkgSpec], [(pkgName, GhcEnvFilePackageId installedUnitId)])
  _ -> ([], [])

-- | Disables tests and benchmarks if they weren't explicitly enabled.
disableTestsBenchsByDefault :: ConfigFlags -> ConfigFlags
disableTestsBenchsByDefault configFlags =
  configFlags
    { configTests = Flag False <> configTests configFlags
    , configBenchmarks = Flag False <> configBenchmarks configFlags
    }

-- | Disables program prefix and suffix, in order to get the /canonical/
-- executable name in the store and thus:
--
-- * avoid making the package hash depend on these options and needless rebuild;
-- * provide the correct executable path to the install methods (copy, symlink).
ignoreProgramAffixes :: ConfigFlags -> ConfigFlags
ignoreProgramAffixes configFlags =
  configFlags
    { configProgPrefix = NoFlag
    , configProgSuffix = NoFlag
    }

-- | Prepares a record containing the information needed to either symlink or
-- copy an executable.
symlink :: OverwritePolicy -> InstallExe -> UnitId -> UnqualComponentName -> Symlink
symlink
  overwritePolicy
  InstallExe{installDir, mkSourceBinDir, mkExeName, mkFinalExeName}
  unit
  exe =
    Symlink
      overwritePolicy
      installDir
      (mkSourceBinDir unit)
      (mkFinalExeName exe)
      (mkExeName exe)

-- |
-- -- * When 'InstallCheckOnly', warn if install would fail overwrite policy
--      checks but don't install anything.
-- -- * When 'InstallCheckInstall', try to symlink or copy every package exe
--      from the store to a given location. When not permitted by the overwrite
--      policy, stop with a message.
installCheckUnitExes :: InstallCheck -> InstallAction
installCheckUnitExes
  installCheck
  verbosity
  overwritePolicy
  installExe@InstallExe{installMethod, installDir, mkSourceBinDir, mkExeName, mkFinalExeName}
  (unit, components) = do
    symlinkables :: [Bool] <- traverse (symlinkableBinary . symlink overwritePolicy installExe unit) exes
    case installCheck of
      InstallCheckOnly -> traverse_ warnAbout (zip symlinkables exes)
      InstallCheckInstall ->
        if and symlinkables
          then traverse_ installAndWarn exes
          else traverse_ warnAbout (zip symlinkables exes)
    where
      exes = catMaybes $ (exeMaybe . fst) <$> components
      exeMaybe (ComponentTarget (CExeName exe) _) = Just exe
      exeMaybe _ = Nothing

      warnAbout (True, _) = return ()
      warnAbout (False, exe) = dieWithException verbosity $ InstallUnitExes (errorMessage installDir exe)

      installAndWarn exe = do
        success <-
          installBuiltExe
            verbosity
            overwritePolicy
            (mkSourceBinDir unit)
            (mkExeName exe)
            (mkFinalExeName exe)
            installDir
            installMethod
        unless success $ dieWithException verbosity $ InstallUnitExes (errorMessage installDir exe)

      errorMessage installdir exe = case overwritePolicy of
        NeverOverwrite ->
          "Path '"
            <> (installdir </> mkFinalExeName exe)
            <> "' already exists. "
            <> "Use --overwrite-policy=always to overwrite."
        -- This shouldn't even be possible, but we keep it in case symlinking or
        -- copying logic changes.
        _ ->
          case installMethod of
            InstallMethodSymlink -> "Symlinking"
            InstallMethodCopy -> "Copying" <> " '" <> prettyShow exe <> "' failed."

-- | Install a specific exe.
installBuiltExe
  :: Verbosity
  -> OverwritePolicy
  -> FilePath
  -- ^ The directory where the built exe is located
  -> FilePath
  -- ^ The exe's filename
  -> FilePath
  -- ^ The exe's filename in the public install directory
  -> FilePath
  -- ^ the directory where it should be installed
  -> InstallMethod
  -> IO Bool
  -- ^ Whether the installation was successful
installBuiltExe
  verbosity
  overwritePolicy
  sourceDir
  exeName
  finalExeName
  installdir
  InstallMethodSymlink = do
    notice verbosity $ "Symlinking '" <> exeName <> "' to '" <> destination <> "'"
    symlinkBinary
      ( Symlink
          overwritePolicy
          installdir
          sourceDir
          finalExeName
          exeName
      )
    where
      destination = installdir </> finalExeName
installBuiltExe
  verbosity
  overwritePolicy
  sourceDir
  exeName
  finalExeName
  installdir
  InstallMethodCopy = do
    notice verbosity $ "Copying '" <> exeName <> "' to '" <> destination <> "'"
    exists <- doesPathExist destination
    case (exists, overwritePolicy) of
      (True, NeverOverwrite) -> pure False
      (True, AlwaysOverwrite) -> overwrite
      (True, PromptOverwrite) -> maybeOverwrite
      (False, _) -> copy
    where
      source = sourceDir </> exeName
      destination = installdir </> finalExeName
      remove = do
        isDir <- doesDirectoryExist destination
        if isDir
          then removeDirectory destination
          else removeFile destination
      copy = copyFile source destination >> pure True
      overwrite :: IO Bool
      overwrite = remove >> copy
      maybeOverwrite :: IO Bool
      maybeOverwrite =
        promptRun
          "Existing file found while installing executable. Do you want to overwrite that file? (y/n)"
          overwrite

-- | Create 'GhcEnvironmentFileEntry's for packages with exposed libraries.
entriesForLibraryComponents :: TargetsMap -> [GhcEnvironmentFileEntry FilePath]
entriesForLibraryComponents = Map.foldrWithKey' (\k v -> mappend (go k v)) []
  where
    hasLib :: (ComponentTarget, NonEmpty TargetSelector) -> Bool
    hasLib (ComponentTarget (CLibName _) _, _) = True
    hasLib _ = False

    go
      :: UnitId
      -> [(ComponentTarget, NonEmpty TargetSelector)]
      -> [GhcEnvironmentFileEntry FilePath]
    go unitId targets
      | any hasLib targets = [GhcEnvFilePackageId unitId]
      | otherwise = []

-- | Gets the file path to the request environment file. The @Bool@ is @True@
-- if we got an explicit instruction using @--package-env@, @False@ if we used
-- the default.
getEnvFile :: ClientInstallFlags -> Platform -> Version -> IO (Bool, FilePath)
getEnvFile clientInstallFlags platform compilerVersion = do
  appDir <- getGhcAppDir
  case flagToMaybe (cinstEnvironmentPath clientInstallFlags) of
    Just spec
      -- Is spec a bare word without any "pathy" content, then it refers to
      -- a named global environment.
      | takeBaseName spec == spec ->
          return (True, getGlobalEnv appDir platform compilerVersion spec)
      | otherwise -> do
          spec' <- makeAbsolute spec
          isDir <- doesDirectoryExist spec'
          if isDir
            then -- If spec is a directory, then make an ambient environment inside
            -- that directory.
              return (True, getLocalEnv spec' platform compilerVersion)
            else -- Otherwise, treat it like a literal file path.
              return (True, spec')
    Nothing ->
      return (False, getGlobalEnv appDir platform compilerVersion "default")

-- | Returns the list of @GhcEnvFilePackageId@ values already existing in the
--   environment being operated on. The @Bool@ is @True@ if we took settings
--   from an existing file, @False@ otherwise.
getExistingEnvEntries :: Verbosity -> CompilerFlavor -> Bool -> FilePath -> IO (Bool, [GhcEnvironmentFileEntry FilePath])
getExistingEnvEntries verbosity compilerFlavor supportsPkgEnvFiles envFile = do
  envFileExists <- doesFileExist envFile
  (usedExisting, allEntries) <-
    if (compilerFlavor == GHC || compilerFlavor == GHCJS)
      && supportsPkgEnvFiles
      && envFileExists
      then catch ((True,) <$> readGhcEnvironmentFile envFile) $ \(_ :: ParseErrorExc) ->
        warn
          verbosity
          ( "The environment file "
              ++ envFile
              ++ " is unparsable. Libraries cannot be installed."
          )
          >> return (False, [])
      else return (False, [])
  return (usedExisting, filterEnvEntries allEntries)
  where
    -- Why? We know what the first part will be, we only care about the packages.
    filterEnvEntries = filter $ \case
      GhcEnvFilePackageId _ -> True
      _ -> False

-- | Constructs the path to the global GHC environment file.
--
-- TODO(m-renaud): Create PkgEnvName newtype wrapper.
getGlobalEnv :: FilePath -> Platform -> Version -> String -> FilePath
getGlobalEnv appDir platform compilerVersion name =
  appDir
    </> ghcPlatformAndVersionString platform compilerVersion
    </> "environments"
    </> name

-- | Constructs the path to a local GHC environment file.
getLocalEnv :: FilePath -> Platform -> Version -> FilePath
getLocalEnv dir platform compilerVersion =
  dir
    </> ".ghc.environment."
    <> ghcPlatformAndVersionString platform compilerVersion

getPackageDbStack
  :: Compiler
  -> Flag FilePath
  -> Flag FilePath
  -> [Maybe PackageDBCWD]
  -> IO PackageDBStackCWD
getPackageDbStack compiler storeDirFlag logsDirFlag packageDbs = do
  mstoreDir <- traverse makeAbsolute $ flagToMaybe storeDirFlag
  let
    mlogsDir = flagToMaybe logsDirFlag
  cabalLayout <- mkCabalDirLayout mstoreDir mlogsDir
  let storePackageDBStack = interpretPackageDbFlags False packageDbs ++ [storePackageDB (cabalStoreDirLayout cabalLayout) compiler]
  pure storePackageDBStack

-- | This defines what a 'TargetSelector' means for the @bench@ command.
-- It selects the 'AvailableTarget's that the 'TargetSelector' refers to,
-- or otherwise classifies the problem.
--
-- For the @build@ command select all components except non-buildable
-- and disabled tests\/benchmarks, fail if there are no such
-- components
selectPackageTargets
  :: TargetSelector
  -> [AvailableTarget k]
  -> Either TargetProblem' [k]
selectPackageTargets targetSelector targets
  -- If there are any buildable targets then we select those
  | not (null targetsBuildable) =
      Right targetsBuildable
  -- If there are targets but none are buildable then we report those
  | not (null targets) =
      Left (TargetProblemNoneEnabled targetSelector targets')
  -- If there are no targets at all then we report that
  | otherwise =
      Left (TargetProblemNoTargets targetSelector)
  where
    targets' = forgetTargetsDetail targets
    targetsBuildable =
      selectBuildableTargetsWith
        (buildable targetSelector)
        targets

    -- When there's a target filter like "pkg:tests" then we do select tests,
    -- but if it's just a target like "pkg" then we don't build tests unless
    -- they are requested by default (i.e. by using --enable-tests)
    buildable (TargetPackage _ _ Nothing) TargetNotRequestedByDefault = False
    buildable (TargetAllPackages Nothing) TargetNotRequestedByDefault = False
    buildable _ _ = True

-- | For a 'TargetComponent' 'TargetSelector', check if the component can be
-- selected.
--
-- For the @build@ command we just need the basic checks on being buildable etc.
selectComponentTarget
  :: SubComponentTarget
  -> AvailableTarget k
  -> Either TargetProblem' k
selectComponentTarget = selectComponentTargetBasic

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity problems = reportTargetProblems verbosity "build" problems

reportCannotPruneDependencies :: Verbosity -> CannotPruneDependencies -> IO a
reportCannotPruneDependencies verbosity =
  dieWithException verbosity . SelectComponentTargetError . renderCannotPruneDependencies
